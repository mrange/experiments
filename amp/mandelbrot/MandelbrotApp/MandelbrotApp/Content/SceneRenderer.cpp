#include "pch.h"
#include "SceneRenderer.h"

#include "ShaderStructures.h"

#include <algorithm>
#include <utility>

using namespace concurrency;
using namespace concurrency::graphics;
using namespace concurrency::fast_math;

using namespace MandelbrotApp;
using namespace Microsoft::WRL;
using namespace DirectX;
using namespace Windows::Foundation;
using namespace Windows::System;

namespace
{
    typedef float mtype;

    struct ModelViewProjectionConstantBuffer
    {
        XMFLOAT4X4 model;
        XMFLOAT4X4 view;
        XMFLOAT4X4 projection;
    };

    struct MandelbrotPos
    {
        XMFLOAT3 pos   ;
        XMFLOAT3 normal;
        XMFLOAT2 texpos;
    };

    struct MandelbrotPoint
    {
        mtype   x;
        mtype   y;

        MandelbrotPoint(mtype x, mtype y)
            : x(x)
            , y(y)
        {
        }
    };



    int             const   step_iter       = 8         ;
    int             const   max_iter        = 1024      ;
    int             const   min_iter        = 8         ;

    unsigned int    const   texture_width   = 800       ;
    unsigned int    const   texture_height  = 800       ;
    mtype           const   cx_mandelbrot   = -0.75     ;
    mtype           const   cy_mandelbrot   = 0         ;
    mtype           const   zoom_mandelbrot = 1/3.0F    ;

    mtype           const   cx_julia        = 0         ;
    mtype           const   cy_julia        = 0         ;
    mtype           const   zoom_julia      = 1/3.0F    ;

    inline bool test(mtype x, mtype y) restrict(amp, cpu)
    {
        return (x * x + y * y) < 4;
//        return (fabs(x) < 2) & (fabs(y) < 2);
    }

    inline int mandelbrot (mtype x, mtype y, int iter) restrict(amp, cpu)
    {
        auto ix = x;
        auto iy = y;

        auto i = 0;

        for (; (i < iter) & test (ix, iy); ++i)
        {
            auto tx = ix * ix - iy * iy + x;
            iy = 2 * ix * iy + y;
            ix = tx;
        }
        return i;
    }

    inline int julia (mtype x, mtype y, mtype cx, mtype cy, int iter) restrict(amp, cpu)
    {
        auto ix = x;
        auto iy = y;

        auto i = 0;

        for (; (i < iter) & test (ix, iy); ++i)
        {
            auto tx = ix * ix - iy * iy + cx;
            iy = 2 * ix * iy + cy;
            ix = tx;
        }
        return i;
    }

    void fill_color_lookup (std::vector<unorm_4> & result, unorm_4 from, unorm_4 to, std::size_t steps)
    {
        if (steps < 1U)
        {
            return;
        }

        auto diff = norm_4 (to) - norm_4 (from);

        for (auto iter = 0U; iter < steps - 1U; ++iter)
        {
            auto ratio = norm_4 (static_cast<float> (iter) / static_cast<float> (steps));
            result.push_back (unorm_4 (norm_4 (from) + ratio * diff));
        }

        result.push_back (to);
    }


    unorm_4 black   (0.0F, 0.0F, 0.0F, 1.0F);
    unorm_4 white   (1.0F, 1.0F, 1.0F, 1.0F);

    unorm_4 red     (1.0F, 0.0F, 0.0F, 1.0F);
    unorm_4 yellow  (1.0F, 1.0F, 0.0F, 1.0F);
    unorm_4 green   (0.0F, 1.0F, 0.0F, 1.0F);
    unorm_4 cyan    (0.0F, 1.0F, 1.0F, 1.0F);
    unorm_4 blue    (0.0F, 0.0F, 1.0F, 1.0F);
    unorm_4 magenta (1.0F, 0.0F, 1.0F, 1.0F);

    std::vector<unorm_4> create_color_lookup ()
    {
        std::vector<unorm_4> result;

        auto filler = [&] (unorm_4 from, unorm_4 to)
        {
            fill_color_lookup (result, from, to, 16);
        };

        filler (red     , yellow    );
        filler (yellow  , green     );
        filler (green   , cyan      );
        filler (cyan    , blue      );
        filler (blue    , magenta   );
        filler (magenta , red       );


        return result;
    }

    std::vector<unorm_4> const color_lookup = create_color_lookup ();

    template<typename TPredicate>
    void compute_set (
            accelerator_view const &    av
        ,   ID3D11Texture2D *           texture
        ,   int                         offset
        ,   int                         iter
        ,   mtype                       cx
        ,   mtype                       cy
        ,   mtype                       ix
        ,   mtype                       iy
        ,   mtype                       zoom
        ,   TPredicate                  const & predicate
        )
    {
        if (!texture)
        {
            return;
        }

        int const lookup_size   = static_cast<int> (color_lookup.size ());

        auto lookup             = array_view<unorm_4 const, 1> (color_lookup);

        auto tex                = concurrency::graphics::direct3d::make_texture<unorm_4,2>(av, texture);
        auto texv               = texture_view<unorm_4, 2> (tex);
        auto e                  = tex.extent;

        auto width              = static_cast<mtype> (e[1]);
        auto height             = static_cast<mtype> (e[0]);

        auto aspect             = width / height;

        auto dx                 = aspect * 1/zoom       ;
        auto dy                 = 1/zoom                ;

        parallel_for_each (
                av
            ,   e
            ,   [=] (index<2> idx) restrict(amp)
            {
                auto x = cx + dx * ((idx[1] / width) - 0.5F);
                auto y = cy + dy * ((idx[0] / height) - 0.5F);

                auto result = predicate (x,y, ix, iy, iter);

                auto multiplier = result == iter ? 0.0F : 1.0F;

                auto color = unorm_4 (multiplier, multiplier, multiplier, 1.0F) * lookup[(result + offset) % lookup_size];

                texv.set(idx,color); 
            });
    }
}

struct SceneRenderer::Impl
{
    Impl(std::shared_ptr<DeviceResources> const & deviceResources)
        :   m_loadingComplete(false)
        ,   m_degreesPerSecond(45)
        ,   m_indexCount(0)
        ,   m_deviceResources(deviceResources)
        ,   m_center(cx_mandelbrot, cy_mandelbrot)
        ,   m_zoom(zoom_mandelbrot)
        ,   m_iter(128)
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
                &&  m_mandelBrotTexture
                &&  m_mandelBrotTextureView
                &&  m_juliaTexture
                &&  m_juliaTextureView
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
            1,
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

    MandelbrotPoint GetPointFromScreenCoord(Point cp) const
    {
        auto cb = m_currentBounds;

        if (cp.X > cb.Width / 2)
        {
            cp.X = cb.Width / 2;
        }

        auto centerX = cb.Width / 4;
        auto centerY = cb.Height / 2;

        auto cy = ((cp.Y - centerY) / cb.Height) / m_zoom + m_center.y;
        auto cx = ((cp.X - centerX) / cb.Height) / m_zoom + m_center.x;

        return MandelbrotPoint(cx,cy);

    }

    // Called once per frame, rotates the cube and calculates the model and view matrices.
    void Update(DX::StepTimer const& timer)
    {
        if (!IsInitialized())
        {
            return;
        }

        // Eye is at (0,0.7,1.5), looking at point (0,-0.1,0) with the up-vector along the y-axis.
        static const XMVECTORF32 eye = { 0.0f, 0.0f, 1.5f, 0.0f };
        static const XMVECTORF32 at = { 0.0f, 0.0f, 0.0f, 0.0f };
        static const XMVECTORF32 up = { 0.0f, 1.0f, 0.0f, 0.0f };

        auto totalSecs = static_cast<float> (timer.GetTotalSeconds ());

        // Convert degrees to radians, then convert seconds to rotation angle
        float radiansPerSecond = XMConvertToRadians(m_degreesPerSecond);
        double totalRotation = totalSecs * radiansPerSecond;
        totalRotation = 0.0;
        float animRadians = (float)fmod(totalRotation, XM_2PI);

        // Prepare to pass the view matrix, and updated model matrix, to the shader
        XMStoreFloat4x4(&m_constantBufferData.view, XMMatrixTranspose(XMMatrixLookAtRH(eye, at, up)));
        XMStoreFloat4x4(&m_constantBufferData.model, XMMatrixTranspose(XMMatrixRotationY(animRadians)));

        auto coord = GetPointFromScreenCoord(m_currentPoint);

        auto offset = static_cast<int> (totalSecs * 10);

        compute_set (
                *m_av
            ,   m_juliaTexture.Get()
            ,   offset
            ,   m_iter
            ,   cx_julia
            ,   cy_julia
            ,   coord.x
            ,   coord.y
            ,   zoom_julia
            ,   [=](mtype x, mtype y, mtype cx, mtype cy, int iter) restrict(amp) {return julia(x,y,cx,cy,iter);}
            );

        compute_set (
                *m_av
            ,   m_mandelBrotTexture.Get()
            ,   offset
            ,   m_iter
            ,   m_center.x
            ,   m_center.y
            ,   m_center.x
            ,   m_center.y
            ,   m_zoom
            ,   [=](mtype x, mtype y, mtype cx, mtype cy, int iter) restrict(amp) {return mandelbrot(x,y,iter);}
            );

        uint32 fps = timer.GetFramesPerSecond();

        wchar_t text [256];

        auto text_length = swprintf_s(
                text
            ,   L"Mouse wheel to zoom\r\nA,Z:Controls iterations\r\nFPS:%i I:%d\r\nZ:%f\r\nCX:%f CY:%f\r\nX:%f Y:%f"
            ,   fps
            ,   m_iter
            ,   m_zoom
            ,   m_center.x
            ,   m_center.y
            ,   coord.x
            ,   coord.y
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
            UINT stride = sizeof(MandelbrotPos);
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
                m_mandelBrotTextureView.GetAddressOf()
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

            context->PSSetShaderResources(
                0,
                1,
                m_juliaTextureView.GetAddressOf()
                );

            context->PSSetSamplers(
                0,
                1,
                m_sampler.GetAddressOf()
                );

            context->DrawIndexed(
                6,
                6,
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
        textureDesc.Width                   = m_currentBounds.Width / 2 ;
        textureDesc.Height                  = m_currentBounds.Height    ;
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
        auto mandelbrot = CreateTextureAndView();
        auto julia = CreateTextureAndView();

        m_mandelBrotTexture         = mandelbrot.first  ;
        m_mandelBrotTextureView     = mandelbrot.second ;

        m_juliaTexture              = julia.first       ;
        m_juliaTextureView          = julia.second      ;
    }

    void CreateDeviceDependentResources()
    {
        // Load shaders asynchronously.
        auto loadVSTask = DX::ReadDataAsync(L"SampleVertexShader.cso");
        auto loadPSTask = DX::ReadDataAsync(L"SamplePixelShader.cso");

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
            static const MandelbrotPos cubeVertices[] = 
            {
                {XMFLOAT3(-1, -0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 1)},
                {XMFLOAT3( 0, -0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 1)},
                {XMFLOAT3( 0,  0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 0)},
                {XMFLOAT3(-1,  0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 0)},

                {XMFLOAT3( 0, -0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 1)},
                {XMFLOAT3( 1, -0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 1)},
                {XMFLOAT3( 1,  0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 0)},
                {XMFLOAT3( 0,  0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 0)},
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
                0x04 + 2,0x04 + 1,0x04 + 0,
                0x04 + 0,0x04 + 3,0x04 + 2,
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
            m_av = std::make_shared<accelerator_view> (av);


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

        m_av.reset();
        m_vertexShader.Reset();
        m_inputLayout.Reset();
        m_pixelShader.Reset();
        m_constantBuffer.Reset();
        m_vertexBuffer.Reset();
        m_indexBuffer.Reset();
    }

    void PointerWheelChanged(Point const & p, int delta)
    {
        auto mp = GetPointFromScreenCoord(p);

        auto dx = m_center.x - mp.x;
        auto dy = m_center.y - mp.y;

        auto zoomratio = static_cast<mtype> (std::pow(1.1, delta/120.0));

        m_zoom *= zoomratio;

        m_center.x = mp.x + dx/zoomratio;
        m_center.y = mp.y + dy/zoomratio;

        m_currentPoint = p;
    }

    void PointerMoved(Point const & p)
    {
        m_currentPoint = p;
    }

    void KeyUp(VirtualKey vk)
    {
        switch (vk)
        {
        case VirtualKey::A:
            m_iter = std::min(m_iter + step_iter, max_iter);
            break;
        case VirtualKey::Z:
            m_iter = std::max(m_iter - step_iter, min_iter);
            break;
        default:
            break;
        }
    }

    // Cached pointer to device resources.
    std::shared_ptr<DeviceResources>                    m_deviceResources       ;

    std::shared_ptr<Concurrency::accelerator_view>      m_av                    ;

    // Direct3D resources for cube geometry.
    ComPtr<ID3D11InputLayout>           m_inputLayout           ;
    ComPtr<ID3D11Buffer>                m_vertexBuffer          ;
    ComPtr<ID3D11Buffer>                m_indexBuffer           ;
    ComPtr<ID3D11VertexShader>          m_vertexShader          ;
    ComPtr<ID3D11PixelShader>           m_pixelShader           ;
    ComPtr<ID3D11Buffer>                m_constantBuffer        ;

    ComPtr<ID3D11SamplerState>          m_sampler               ;

    ComPtr<ID3D11Texture2D>             m_mandelBrotTexture     ;
    ComPtr<ID3D11ShaderResourceView>    m_mandelBrotTextureView ;

    ComPtr<ID3D11Texture2D>             m_juliaTexture          ;
    ComPtr<ID3D11ShaderResourceView>    m_juliaTextureView      ;

    // System resources for cube geometry.
    ModelViewProjectionConstantBuffer   m_constantBufferData    ;
    uint32                              m_indexCount            ;

    // Variables used with the rendering loop.
    bool                                m_loadingComplete       ;
    float                               m_degreesPerSecond      ;

    MandelbrotPoint                     m_center                ;
    mtype                               m_zoom                  ;
    int                                 m_iter                  ;

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
