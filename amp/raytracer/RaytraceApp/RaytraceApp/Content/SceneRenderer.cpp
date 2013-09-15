#include "pch.h"
#include "SceneRenderer.h"

#include <algorithm>
#include <utility>

using namespace concurrency;
using namespace concurrency::direct3d;
using namespace concurrency::graphics;
using namespace concurrency::fast_math;

using namespace RaytraceApp;
using namespace Microsoft::WRL;
using namespace DirectX;
using namespace Windows::Foundation;
using namespace Windows::System;

#define GPU __GPU_ONLY
#define RAYTRACE_INLINE inline
#define RAYTRACE_CUTOFF 0.000001F

namespace
{

    // ------------------------------------------------------------------------
    // Linear algebra
    // ------------------------------------------------------------------------

    RAYTRACE_INLINE float mad2 (float x, float y, float z) restrict (cpu)
    {
        return x * y  + z;
    }

    RAYTRACE_INLINE float dot (float_2 const & left, float_2 const & right) GPU
    {
        return mad (left.y, right.y, left.x*right.x);
    }

    RAYTRACE_INLINE float dot (float_3 const & left, float_3 const & right) GPU
    {
        return mad (left.z, right.z, mad (left.y, right.y, left.x*right.x));
    }

    RAYTRACE_INLINE float_3 cross (float_3 const & left, float_3 const & right) GPU
    {
        return float_3 (
                mad (left.y, right.z,  -left.z * right.y)
            ,   mad (left.z, right.x,  -left.x * right.z)
            ,   mad (left.x, right.y,  -left.y * right.x)
            );
    }

    RAYTRACE_INLINE float_2 scale (float_2 const & v, float scale) GPU
    {
        return float_2 (v.x * scale, v.y * scale);
    }

    RAYTRACE_INLINE float_3 scale (float_3 const & v, float scale) GPU
    {
        return float_3 (v.x * scale, v.y * scale, v.z * scale);
    }

    template<typename TVector>
    RAYTRACE_INLINE TVector iscale (TVector const & v, float iscale) GPU
    {
        return scale (v, 1.0F / iscale);
    }

    template<typename TVector>
    RAYTRACE_INLINE float l2 (TVector const & v) GPU
    {
        return dot (v, v);
    }

    template<typename TVector>
    RAYTRACE_INLINE float length (TVector const & v) GPU 
    {
        return sqrtf (l2 (v));
    }

    template<typename TVector>
    RAYTRACE_INLINE TVector normalize (TVector const & v) GPU 
    {
        return iscale (v, length (v));
    }

    // ------------------------------------------------------------------------
    // Linear algebra
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Raytracer primitives
    // ------------------------------------------------------------------------

    struct material
    {
        unorm_4     color       ;
        unorm_4     specular    ;
        unorm       diffusion   ;
        unorm       reflection  ;

        RAYTRACE_INLINE material create (
                unorm_4 const & color
            ,   unorm_4 const & specular
            ,   unorm   const & diffusion
            ,   unorm   const & reflection
            ) GPU
        {
            return material
            {
                    color
                ,   specular
                ,   diffusion
                ,   reflection
            };
        }
    };

    struct ray
    {
        float_3     origin      ;
        float_3     direction   ;   // Always unit vector

        RAYTRACE_INLINE static ray from_to (float_3 from, float_3 to) GPU
        {
            return ray 
            {
                    from
                ,   normalize (to - from)
            };
        }

        RAYTRACE_INLINE static ray origin_direction (float_3 origin, float_3 direction) GPU
        {
            return ray 
            {
                    origin
                ,   normalize (direction)
            };
        }

        RAYTRACE_INLINE float_3 trace (float t) const GPU
        {
            return origin + t *direction;
        }

        RAYTRACE_INLINE float_2 intersect_sphere (float_3 const & center, float radius) const GPU
        {
            auto v  = origin - center   ;
            auto vd = dot (v, direction);
            auto v2 = l2 (v)            ;
            auto r2 = radius * radius   ;

            auto d  = vd * vd - v2 + r2 ;

            if (d < 0.0F)
            {
                return float_2 ();      
            }
            else
            {
                auto root   = sqrtf (d) ;
                auto t1     = -vd + root;
                auto t2     = -vd - root;

                if (t1 < RAYTRACE_CUTOFF || t2 < RAYTRACE_CUTOFF)
                {
                    return float_2 ();      
                }
                else if (t1 < t2)
                {
                    return float_2 (t1, t2);
                }
                else
                {
                    return float_2 (t2, t1);
                }
            }
        }

        RAYTRACE_INLINE float intersect_plane (float_3 const & normal, float offset) const GPU
        {
            auto t = -(dot (origin, normal) + offset) / (dot (direction, normal));

            if (t < RAYTRACE_CUTOFF)
            {
                return 0.0F;
            }
            else
            {
                return t;
            }
        }
    };

    struct light_source
    {
        unorm_4     color       ;
        float_3     origin      ;
        float       radius      ;

        RAYTRACE_INLINE light_source create (
                unorm_4 const & color
            ,   float_3 const & origin
            ,   float           radius
            ) GPU
        {
            return light_source
            {
                    color
                ,   origin
                ,   radius
            };
        }
    };


    struct shape
    {
        enum shape_type
        {
                sphere
            ,   plane
        };

        struct sphere_data
        {
            float   center_x;
            float   center_y;
            float   center_z;
            float   radius  ;
        };

        struct plane_data
        {
            float   normal_x;   // normal is unit vector
            float   normal_y;
            float   normal_z;
            float   offset  ;
        };

        shape_type  type            ;
        material    shape_material  ;

        union
        {
            sphere_data sphere  ;
            plane_data  plane   ;
        }           data        ;

        RAYTRACE_INLINE shape create_sphere (
                material    const & material
            ,   float_3     const & center
            ,   float               radius
            ) GPU
        {
            auto s = shape
            {
                    sphere
                ,   material
            };

            s.data.sphere = sphere_data
            {
                    center.x
                ,   center.y
                ,   center.z
                ,   fabs (radius)
            };

            return s;
        }

        RAYTRACE_INLINE shape create_plane (
                material    const & material
            ,   float_3     const & normal
            ,   float               offset
            ) GPU
        {
            auto n = normalize (normal);

            auto s = shape
            {
                    plane
                ,   material
            };

            s.data.plane = plane_data
            {
                    n.x
                ,   n.y
                ,   n.z
                ,   offset
            };
            
            return s;
        }
    };


    // ------------------------------------------------------------------------
    // Raytracer primitives
    // ------------------------------------------------------------------------

    struct ModelViewProjectionConstantBuffer
    {
        XMFLOAT4X4 model;
        XMFLOAT4X4 view;
        XMFLOAT4X4 projection;
    };

    struct ViewPos
    {
        XMFLOAT3 pos   ;
        XMFLOAT3 normal;
        XMFLOAT2 texpos;
    };

    unorm_4 black   (0.0F, 0.0F, 0.0F, 1.0F);
    unorm_4 white   (1.0F, 1.0F, 1.0F, 1.0F);

    unorm_4 red     (1.0F, 0.0F, 0.0F, 1.0F);
    unorm_4 yellow  (1.0F, 1.0F, 0.0F, 1.0F);
    unorm_4 green   (0.0F, 1.0F, 0.0F, 1.0F);
    unorm_4 cyan    (0.0F, 1.0F, 1.0F, 1.0F);
    unorm_4 blue    (0.0F, 0.0F, 1.0F, 1.0F);
    unorm_4 magenta (1.0F, 0.0F, 1.0F, 1.0F);

    void compute (
            accelerator_view const &    av
        ,   ID3D11Texture2D *           texture
        )
    {
        if (!texture)
        {
            return;
        }

        auto tex                = concurrency::graphics::direct3d::make_texture<unorm_4,2>(av, texture);
        auto texv               = texture_view<unorm_4, 2> (tex);
        auto e                  = tex.extent;

        auto width              = static_cast<float> (e[1]);
        auto height             = static_cast<float> (e[0]);

        auto aspect             = width / height        ;
        //auto dx                 = aspect * 1/zoom       ;
        //auto dy                 = 1/zoom                ;

        auto color              = magenta               ;

        parallel_for_each (
                av
            ,   e
            ,   [=] (index<2> idx) restrict (amp)
            {
                texv.set (idx,color); 
            });
    }
}

struct SceneRenderer::Impl
{
    Impl(std::shared_ptr<DeviceResources> const & deviceResources)
        :   m_loadingComplete(false)
        ,   m_indexCount(0)
        ,   m_deviceResources(deviceResources)
    {
        ZeroMemory(&m_textMetrics, sizeof(DWRITE_TEXT_METRICS));

        CreateDeviceDependentResources();
    }

    ~Impl()
    {
    }

    bool IsInitialized()
    {
        return      m_loadingComplete 
                &&  m_raytraceTexture
                &&  m_raytraceTextureView
                ;
    }

    // Initializes view parameters when the window size changes.
    void CreateWindowSizeDependentResources()
    {
        Size outputBounds = m_deviceResources->GetOutputBounds();
        m_currentBounds = outputBounds;

        m_currentPoint = Point(outputBounds.Width / 4, outputBounds.Height / 2);
        float aspectRatio = outputBounds.Width / outputBounds.Height;
        float fovAngleY = 70.0f * XM_PI / 180.0f;

        // This is a simple example of change that can be made when the app is in
        // portrait or snapped view.
        if (aspectRatio < 1.0f)
        {
            fovAngleY *= 2.0f;
        }

        XMMATRIX perspectiveMatrix = XMMatrixOrthographicRH(
            2,
            2,
            -10,
            +10
            );

        XMFLOAT4X4 orientation = m_deviceResources->GetOrientationTransform3D();

        XMMATRIX orientationMatrix = XMLoadFloat4x4(&orientation);

        XMStoreFloat4x4(
            &m_constantBufferData.projection,
            XMMatrixTranspose(perspectiveMatrix * orientationMatrix)
            );

        InitializeTextures();

    }

    // Called once per frame, rotates the cube and calculates the model and view matrices.
    void Update(DX::StepTimer const& timer)
    {
        if (!IsInitialized())
        {
            return;
        }

        // Eye is at (0,0.7,1.5), looking at point (0,-0.1,0) with the up-vector along the y-axis.
        static const XMVECTORF32 eye    = { 0.0f, 0.0f, 1.5f, 0.0f };
        static const XMVECTORF32 at     = { 0.0f, 0.0f, 0.0f, 0.0f };
        static const XMVECTORF32 up     = { 0.0f, 1.0f, 0.0f, 0.0f };

        // Prepare to pass the view matrix, and updated model matrix, to the shader
        XMStoreFloat4x4(&m_constantBufferData.view, XMMatrixTranspose(XMMatrixLookAtRH(eye, at, up)));
        XMStoreFloat4x4(&m_constantBufferData.model, XMMatrixTranspose(XMMatrixRotationY(0.)));

        compute (
                *m_acceleratorView
            ,   m_raytraceTexture.Get()
            );

        uint32 fps = timer.GetFramesPerSecond();

        wchar_t text [256];

        auto text_length = swprintf_s(
                text
            ,   L"FPS:%i"
            ,   fps
            );

        m_textLayout.Reset();

        DX::ThrowIfFailed(
            m_deviceResources->GetDWriteFactory()->CreateTextLayout(
                text,
                text_length,
                m_textFormat.Get(),
                m_currentBounds.Width   / 2, // Max width of the input text.
                m_currentBounds.Height  / 2, // Max height of the input text.
                &m_textLayout
                )
            );

        DX::ThrowIfFailed(
            m_textLayout->GetMetrics(&m_textMetrics)
            );

    }

    // Renders one frame using the vertex and pixel shaders.
    void Render()
    {
        // Loading is asynchronous. Only draw geometry after it's loaded.
        if (!IsInitialized())
        {
            return;
        }

        {
            auto context = m_deviceResources->GetD3DDeviceContext();

            // Set render targets to the screen.
            ID3D11RenderTargetView *const targets[1] = { m_deviceResources->GetBackBufferRenderTargetView() };

            context->OMSetRenderTargets(1, targets, m_deviceResources->GetDepthStencilView());

            // Prepare the constant buffer to send it to the Graphics device.
            context->UpdateSubresource(
                m_constantBuffer.Get(),
                0,
                NULL,
                &m_constantBufferData,
                0,
                0
                );

            // Each vertex is one instance of the VertexPositionColor struct.
            UINT stride = sizeof(ViewPos);
            UINT offset = 0;
            context->IASetVertexBuffers(
                0,
                1,
                m_vertexBuffer.GetAddressOf(),
                &stride,
                &offset
                );

            context->IASetIndexBuffer(
                m_indexBuffer.Get(),
                DXGI_FORMAT_R16_UINT, // Each index is one 16-bit unsigned integer (short).
                0
                );

            context->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

            context->IASetInputLayout(m_inputLayout.Get());

            // Attach our vertex shader.
            context->VSSetShader(
                m_vertexShader.Get(),
                nullptr,
                0
                );

            // Send the constant buffer to the Graphics device.
            context->VSSetConstantBuffers(
                0,
                1,
                m_constantBuffer.GetAddressOf()
                );

            // Attach our pixel shader.
            context->PSSetShader(
                m_pixelShader.Get(),
                nullptr,
                0
                );

            context->PSSetShaderResources(
                0,
                1,
                m_raytraceTextureView.GetAddressOf()
                );

            context->PSSetSamplers(
                0,
                1,
                m_sampler.GetAddressOf()
                );

            // Draw the objects.
            context->DrawIndexed(
                6,
                0,
                0
                );
        }

        {
            ID2D1DeviceContext* context = m_deviceResources->GetD2DDeviceContext();
            Windows::Foundation::Size outputBounds = m_deviceResources->GetOutputBounds();

            context->SaveDrawingState(m_stateBlock.Get());
            context->BeginDraw();

            // Position on the bottom right corner
            D2D1::Matrix3x2F screenTranslation = D2D1::Matrix3x2F::Translation(
                outputBounds.Width - m_textMetrics.layoutWidth,
                outputBounds.Height - m_textMetrics.height
                );

            context->SetTransform(screenTranslation * m_deviceResources->GetOrientationTransform2D());

            DX::ThrowIfFailed(
                m_textFormat->SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING)
                );

            context->DrawTextLayout(
                D2D1::Point2F(0.f, 0.f),
                m_textLayout.Get(),
                m_whiteBrush.Get()
                );

            // We ignore D2DERR_RECREATE_TARGET here. This error indicates that the device
            // is lost. It will be handled during the next call to Present.
            HRESULT hr = context->EndDraw();
            if (hr != D2DERR_RECREATE_TARGET)
            {
                DX::ThrowIfFailed(hr);
            }

            context->RestoreDrawingState(m_stateBlock.Get());
        }
    }

    std::pair<ComPtr<ID3D11Texture2D>, ComPtr<ID3D11ShaderResourceView>> CreateTextureAndView()
    {
        ComPtr<ID3D11Texture2D> texture;
        D3D11_TEXTURE2D_DESC textureDesc    = {};
        textureDesc.Width                   = static_cast<UINT> (m_currentBounds.Width / 2 );
        textureDesc.Height                  = static_cast<UINT> (m_currentBounds.Height    );
        textureDesc.Format                  = DXGI_FORMAT_R8G8B8A8_UNORM;
        textureDesc.Usage                   = D3D11_USAGE_DEFAULT;
        textureDesc.CPUAccessFlags          = 0;
        textureDesc.MiscFlags               = 0;
        textureDesc.MipLevels               = 1;
        textureDesc.ArraySize               = 1;
        textureDesc.SampleDesc.Count        = 1;
        textureDesc.SampleDesc.Quality      = 0;
        textureDesc.BindFlags               = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_UNORDERED_ACCESS ;

        DX::ThrowIfFailed(
            m_deviceResources->GetD3DDevice()->CreateTexture2D(
                    &textureDesc
                ,   NULL
                ,   &texture
                )
            );

        ComPtr<ID3D11ShaderResourceView> view;

        D3D11_SHADER_RESOURCE_VIEW_DESC textureViewDesc = {};
        textureViewDesc.Format                          = textureDesc.Format;
        textureViewDesc.ViewDimension                   = D3D11_SRV_DIMENSION_TEXTURE2D;
        textureViewDesc.Texture2D.MipLevels             = textureDesc.MipLevels;
        textureViewDesc.Texture2D.MostDetailedMip       = 0;

        DX::ThrowIfFailed(
            m_deviceResources->GetD3DDevice()->CreateShaderResourceView(
                texture.Get(),
                &textureViewDesc,
                &view
                )
            );

        return std::make_pair(texture, view);
    }

    void InitializeTextures()
    {
        auto tv                 = CreateTextureAndView();

        m_raytraceTexture       = tv.first              ;
        m_raytraceTextureView   = tv.second             ;
    }

    void CreateDeviceDependentResources()
    {
        // Load shaders asynchronously.
        auto loadVSTask = DX::ReadDataAsync(L"SceneVertexShader.cso");
        auto loadPSTask = DX::ReadDataAsync(L"ScenePixelShader.cso");

        // After the vertex shader file is loaded, create the shader and input layout.
        auto createVSTask = loadVSTask.then([this](const std::vector<byte>& fileData) {
            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreateVertexShader(
                &fileData[0],
                fileData.size(),
                nullptr,
                &m_vertexShader
                )
                );

            static const D3D11_INPUT_ELEMENT_DESC vertexDesc [] =
            {
                { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0,  0, D3D11_INPUT_PER_VERTEX_DATA, 0 },
                { "NORMAL"  , 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0 },
                { "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT,    0, 24, D3D11_INPUT_PER_VERTEX_DATA, 0 },
            };

            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreateInputLayout(
                vertexDesc,
                ARRAYSIZE(vertexDesc),
                &fileData[0],
                fileData.size(),
                &m_inputLayout
                )
                );
        });

        // After the pixel shader file is loaded, create the shader and constant buffer.
        auto createPSTask = loadPSTask.then([this](const std::vector<byte>& fileData) {
            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreatePixelShader(
                &fileData[0],
                fileData.size(),
                nullptr,
                &m_pixelShader
                )
                );

            CD3D11_BUFFER_DESC constantBufferDesc(sizeof(ModelViewProjectionConstantBuffer) , D3D11_BIND_CONSTANT_BUFFER);
            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreateBuffer(
                &constantBufferDesc,
                nullptr,
                &m_constantBuffer
                )
                );
        });

        // Once both shaders are loaded, create the mesh.
        auto createTask = (createPSTask && createVSTask).then([this] () {

            D3D11_SAMPLER_DESC samplerDesc                  = {};

            samplerDesc.Filter                              = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
            samplerDesc.MaxAnisotropy                       = 0;
            samplerDesc.AddressU                            = D3D11_TEXTURE_ADDRESS_WRAP;
            samplerDesc.AddressV                            = D3D11_TEXTURE_ADDRESS_WRAP;
            samplerDesc.AddressW                            = D3D11_TEXTURE_ADDRESS_WRAP;
            samplerDesc.MipLODBias                          = 0.0f;
            samplerDesc.MinLOD                              = 0;
            samplerDesc.MaxLOD                              = D3D11_FLOAT32_MAX;
            samplerDesc.ComparisonFunc                      = D3D11_COMPARISON_NEVER;
            samplerDesc.BorderColor[0]                      = 0.0f;
            samplerDesc.BorderColor[1]                      = 0.0f;
            samplerDesc.BorderColor[2]                      = 0.0f;
            samplerDesc.BorderColor[3]                      = 0.0f;

            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreateSamplerState(
                    &samplerDesc,
                    &m_sampler
                    )
                );

            // Load mesh vertices. Each vertex has a position and a color.
            static const ViewPos cubeVertices[] = 
            {
                {XMFLOAT3( -1, -1, 0), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 1)},
                {XMFLOAT3(  1, -1, 0), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 1)},
                {XMFLOAT3(  1,  1, 0), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 0)},
                {XMFLOAT3( -1,  1, 0), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 0)},
            };

            D3D11_SUBRESOURCE_DATA vertexBufferData = {0};
            vertexBufferData.pSysMem = cubeVertices;
            vertexBufferData.SysMemPitch = 0;
            vertexBufferData.SysMemSlicePitch = 0;
            CD3D11_BUFFER_DESC vertexBufferDesc(sizeof(cubeVertices), D3D11_BIND_VERTEX_BUFFER);
            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreateBuffer(
                &vertexBufferDesc,
                &vertexBufferData,
                &m_vertexBuffer
                )
                );

            // Load mesh indices. Each triple of indices represents
            // a triangle to be rendered on the screen.
            // For example, 0,2,1 means that the vertices with indexes
            // 0, 2 and 1 from the vertex buffer compose the 
            // first triangle of this mesh.
            static const unsigned short cubeIndices [] =
            {
                0x00 + 2,0x00 + 1,0x00 + 0,
                0x00 + 0,0x00 + 3,0x00 + 2,
            };

            m_indexCount = ARRAYSIZE(cubeIndices);

            D3D11_SUBRESOURCE_DATA indexBufferData = {0};
            indexBufferData.pSysMem = cubeIndices;
            indexBufferData.SysMemPitch = 0;
            indexBufferData.SysMemSlicePitch = 0;
            CD3D11_BUFFER_DESC indexBufferDesc(sizeof(cubeIndices), D3D11_BIND_INDEX_BUFFER);
            DX::ThrowIfFailed(
                m_deviceResources->GetD3DDevice()->CreateBuffer(
                &indexBufferDesc,
                &indexBufferData,
                &m_indexBuffer
                ));

            auto av = concurrency::direct3d::create_accelerator_view (m_deviceResources->GetD3DDevice());
            m_acceleratorView = std::make_shared<accelerator_view> (av);


            DX::ThrowIfFailed(
                m_deviceResources->GetD2DDeviceContext()->CreateSolidColorBrush(D2D1::ColorF(D2D1::ColorF::White), &m_whiteBrush)
                );

            DX::ThrowIfFailed(
                m_deviceResources->GetDWriteFactory()->CreateTextFormat(
                L"Consolas",
                nullptr,
                DWRITE_FONT_WEIGHT_LIGHT,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                24.0f,
                L"en-US",
                &m_textFormat
                )
                );

            DX::ThrowIfFailed(
                m_textFormat->SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR)
                );

            DX::ThrowIfFailed(
                m_deviceResources->GetD2DFactory()->CreateDrawingStateBlock(&m_stateBlock)
                );

        });

        // Once the cube is loaded, the object is ready to be rendered.
        createTask.then([this] () {
            m_loadingComplete = true;
        });
    }

    void ReleaseDeviceDependentResources()
    {
        m_loadingComplete = false;

        m_whiteBrush.Reset();
        m_textFormat.Reset();
        m_stateBlock.Reset();

        m_acceleratorView.reset();
        m_vertexShader.Reset();
        m_inputLayout.Reset();
        m_pixelShader.Reset();
        m_constantBuffer.Reset();
        m_vertexBuffer.Reset();
        m_indexBuffer.Reset();
    }

    void PointerWheelChanged(Point const & p, int delta)
    {
    }

    void PointerMoved(Point const & p)
    {
    }

    void KeyUp(VirtualKey vk)
    {
    }

    // Cached pointer to device resources.
    std::shared_ptr<DeviceResources>                    m_deviceResources       ;

    std::shared_ptr<Concurrency::accelerator_view>      m_acceleratorView       ;

    // Direct3D resources for cube geometry.
    ComPtr<ID3D11InputLayout>           m_inputLayout           ;
    ComPtr<ID3D11Buffer>                m_vertexBuffer          ;
    ComPtr<ID3D11Buffer>                m_indexBuffer           ;
    ComPtr<ID3D11VertexShader>          m_vertexShader          ;
    ComPtr<ID3D11PixelShader>           m_pixelShader           ;
    ComPtr<ID3D11Buffer>                m_constantBuffer        ;

    ComPtr<ID3D11SamplerState>          m_sampler               ;

    ComPtr<ID3D11Texture2D>             m_raytraceTexture       ;
    ComPtr<ID3D11ShaderResourceView>    m_raytraceTextureView   ;

    // System resources for cube geometry.
    ModelViewProjectionConstantBuffer   m_constantBufferData    ;
    uint32                              m_indexCount            ;

    // Variables used with the rendering loop.
    bool                                m_loadingComplete       ;

    Point                               m_currentPoint          ;
    Size                                m_currentBounds         ;

    DWRITE_TEXT_METRICS                 m_textMetrics           ;
    ComPtr<ID2D1SolidColorBrush>        m_whiteBrush            ;
    ComPtr<ID2D1DrawingStateBlock>      m_stateBlock            ;
    ComPtr<IDWriteTextLayout>           m_textLayout            ;
    ComPtr<IDWriteTextFormat>           m_textFormat            ;

};

// Loads vertex and pixel shaders from files and instantiates the cube geometry.
SceneRenderer::SceneRenderer(const std::shared_ptr<DeviceResources>& deviceResources)
    : m_impl(std::make_unique<Impl> ((deviceResources)))
{
}

SceneRenderer::~SceneRenderer()
{
}

void SceneRenderer::CreateDeviceDependentResources()
{
    m_impl->CreateDeviceDependentResources();
}

void SceneRenderer::CreateWindowSizeDependentResources()
{
    m_impl->CreateWindowSizeDependentResources();
}
void SceneRenderer::ReleaseDeviceDependentResources()
{
    m_impl->ReleaseDeviceDependentResources();
}
void SceneRenderer::Update(DX::StepTimer const& timer)
{
    m_impl->Update(timer);
}
void SceneRenderer::Render()
{
    m_impl->Render();
}

void SceneRenderer::PointerWheelChanged(Point const & p, int delta)
{
    m_impl->PointerWheelChanged(p, delta);
}
void SceneRenderer::PointerMoved(Point const & p)
{
    m_impl->PointerMoved(p);
}

void SceneRenderer::KeyUp(VirtualKey vk)
{
    m_impl->KeyUp(vk);
}
