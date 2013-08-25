#include "pch.h"
#include "Sample3DSceneRenderer.h"

#include "..\Common\DirectXHelper.h"	// For ThrowIfFailed and ReadDataAsync

using namespace concurrency;
using namespace concurrency::graphics;

using namespace MandelbrotApp;

using namespace DirectX;
using namespace Windows::Foundation;

namespace
{

    struct MandelBrotPos
    {
	    DirectX::XMFLOAT3 pos   ;
	    DirectX::XMFLOAT3 normal;
	    DirectX::XMFLOAT2 texpos;
    };

    typedef double mtype;

    int mandelbrot (mtype x, mtype y, int iter) restrict(amp, cpu)
    {
        auto ix = x;
        auto iy = y;

        auto i = 0;

        for (; (i < iter) & ((ix * ix + iy * iy) < 4); ++i)
        {
            auto tx = ix * ix - iy * iy + x;
            iy = 2 * ix * iy + y;
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
            fill_color_lookup (result, from, to, 32);
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

    void compute_mandelbrot (
            accelerator_view const &    av
        ,   ID3D11Texture2D *           texture
        ,   int                         offset
        ,   float                       zoom
        )
    {
        if (!texture)
        {
            return;
        }

        try
        {

            unsigned int const  iter    = 256                   ;

            float        const  cx      = 0.001643721971153F    ;
            float        const  cy      = 0.822467633298876F    ;

            float        const  dx      = zoom                  ;
            float        const  dy      = zoom                  ;

            {
                int const lookup_size   = static_cast<int> (color_lookup.size ());

                auto lookup             = array_view<unorm_4 const, 1> (color_lookup);

                auto tex                = concurrency::graphics::direct3d::make_texture<unorm_4,2>(av, texture);
                auto wotex              = texture_view<unorm_4, 2> (tex);
                auto e                  = tex.extent;

                auto width              = static_cast<mtype> (e[0]);
                auto height             = static_cast<mtype> (e[1]);

                parallel_for_each (
                        av
                    ,   e
                    ,   [=] (index<2> idx) restrict(amp)
                    {
                        auto x = cx + dx * ((idx[0] / width) - 0.5);
                        auto y = cy + dy * ((idx[1] / height) - 0.5);

                        auto result = mandelbrot (x,y, iter);

                        auto multiplier = result == iter ? 0.0F : 1.0F;

                        auto color = unorm_4 (multiplier, multiplier, multiplier, 1.0F) * lookup[(result + offset) % lookup_size];

                        wotex.set(idx,color); 
                    });
            }
        }
        catch (std::exception const & e)
        {
            auto what = e.what();
            OutputDebugString(L"Known exception\r\n");
        }
        catch (...)
        {
            OutputDebugString(L"Unknown exception\r\n");
        }
    }
}

// Loads vertex and pixel shaders from files and instantiates the cube geometry.
Sample3DSceneRenderer::Sample3DSceneRenderer(const std::shared_ptr<DeviceResources>& deviceResources) :
	m_loadingComplete(false),
	m_degreesPerSecond(45),
	m_indexCount(0),
	m_deviceResources(deviceResources)
{
	CreateDeviceDependentResources();
}

// Initializes view parameters when the window size changes.
void Sample3DSceneRenderer::CreateWindowSizeDependentResources()
{
	Size outputBounds = m_deviceResources->GetOutputBounds();
	float aspectRatio = outputBounds.Width / outputBounds.Height;
	float fovAngleY = 70.0f * XM_PI / 180.0f;

	// This is a simple example of change that can be made when the app is in
	// portrait or snapped view.
	if (aspectRatio < 1.0f)
	{
		fovAngleY *= 2.0f;
	}

	// Note that the OrientationTransform3D matrix is post-multiplied here
	// in order to correctly orient the scene to match the display orientation.
	// This post-multiplication step is required for any draw calls that are
	// made to the swap chain render target. For draw calls to other targets,
	// this transform should not be applied.

	// This sample makes use of a right-handed coordinate system using row-major matrices.
	XMMATRIX perspectiveMatrix = XMMatrixPerspectiveFovRH(
		fovAngleY,
		aspectRatio,
		0.01f,
		100.0f
		);

	XMFLOAT4X4 orientation = m_deviceResources->GetOrientationTransform3D();

	XMMATRIX orientationMatrix = XMLoadFloat4x4(&orientation);

	XMStoreFloat4x4(
		&m_constantBufferData.projection,
		XMMatrixTranspose(perspectiveMatrix * orientationMatrix)
		);
}

// Called once per frame, rotates the cube and calculates the model and view matrices.
void Sample3DSceneRenderer::Update(DX::StepTimer const& timer)
{
	// Eye is at (0,0.7,1.5), looking at point (0,-0.1,0) with the up-vector along the y-axis.
	static const XMVECTORF32 eye = { 0.0f, 1.0f, 2.0f, 0.0f };
	static const XMVECTORF32 at = { 0.0f, 0.2f, 0.0f, 0.0f };
	static const XMVECTORF32 up = { 0.0f, 1.0f, 0.0f, 0.0f };

    auto totalSecs = static_cast<float> (timer.GetTotalSeconds ());

	// Convert degrees to radians, then convert seconds to rotation angle
	float radiansPerSecond = XMConvertToRadians(m_degreesPerSecond);
	double totalRotation = totalSecs * radiansPerSecond;
	float animRadians = (float)fmod(totalRotation, XM_2PI);

	// Prepare to pass the view matrix, and updated model matrix, to the shader
	XMStoreFloat4x4(&m_constantBufferData.view, XMMatrixTranspose(XMMatrixLookAtRH(eye, at, up)));
	XMStoreFloat4x4(&m_constantBufferData.model, XMMatrixTranspose(XMMatrixRotationY(animRadians)));

    auto zoom = 2.0F / static_cast<float> (pow(1.2, totalSecs));

    compute_mandelbrot (
            *m_av
        ,   m_mandelBrotTexture.Get()
        ,   static_cast<int> (totalSecs * 10)
        ,   zoom
        );
}

// Renders one frame using the vertex and pixel shaders.
void Sample3DSceneRenderer::Render()
{
	// Loading is asynchronous. Only draw geometry after it's loaded.
	if (!m_loadingComplete)
	{
		return;
	}

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
	UINT stride = sizeof(MandelBrotPos);
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
        m_mandelBrotSampler.GetAddressOf()
        );

	// Draw the objects.
	context->DrawIndexed(
		m_indexCount,
		0,
		0
		);
}

void Sample3DSceneRenderer::CreateDeviceDependentResources()
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
	auto createCubeTask = (createPSTask && createVSTask).then([this] () {

        D3D11_TEXTURE2D_DESC textureDesc    = {};
        textureDesc.Width                   = 512;
        textureDesc.Height                  = 512;
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
                ,   &m_mandelBrotTexture
                )
            );

        D3D11_SHADER_RESOURCE_VIEW_DESC textureViewDesc = {};
        textureViewDesc.Format                          = textureDesc.Format;
        textureViewDesc.ViewDimension                   = D3D11_SRV_DIMENSION_TEXTURE2D;
        textureViewDesc.Texture2D.MipLevels             = textureDesc.MipLevels;
        textureViewDesc.Texture2D.MostDetailedMip       = 0;

        DX::ThrowIfFailed(
            m_deviceResources->GetD3DDevice()->CreateShaderResourceView(
                m_mandelBrotTexture.Get(),
                &textureViewDesc,
                &m_mandelBrotTextureView
                )
            );

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
                &m_mandelBrotSampler
                )
            );

		// Load mesh vertices. Each vertex has a position and a color.
		static const MandelBrotPos cubeVertices[] = 
		{
            {XMFLOAT3(-0.5f, -0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 1)},
			{XMFLOAT3( 0.5f, -0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 1)},
			{XMFLOAT3( 0.5f,  0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 0)},
			{XMFLOAT3(-0.5f,  0.5f, 0.5f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 0)},

            {XMFLOAT3(-0.5f, -0.5f,-0.5f), XMFLOAT3( 0.0f, 0.0f, 1.0f), XMFLOAT2( 0, 1)},
			{XMFLOAT3( 0.5f, -0.5f,-0.5f), XMFLOAT3( 0.0f, 0.0f, 1.0f), XMFLOAT2( 1, 1)},
			{XMFLOAT3( 0.5f,  0.5f,-0.5f), XMFLOAT3( 0.0f, 0.0f, 1.0f), XMFLOAT2( 1, 0)},
			{XMFLOAT3(-0.5f,  0.5f,-0.5f), XMFLOAT3( 0.0f, 0.0f, 1.0f), XMFLOAT2( 0, 0)},

            {XMFLOAT3(-0.5f, 0.5f, -0.5f), XMFLOAT3( 0.0f,-1.0f, 0.0f), XMFLOAT2( 0, 1)},
			{XMFLOAT3( 0.5f, 0.5f, -0.5f), XMFLOAT3( 0.0f,-1.0f, 0.0f), XMFLOAT2( 1, 1)},
			{XMFLOAT3( 0.5f, 0.5f,  0.5f), XMFLOAT3( 0.0f,-1.0f, 0.0f), XMFLOAT2( 1, 0)},
			{XMFLOAT3(-0.5f, 0.5f,  0.5f), XMFLOAT3( 0.0f,-1.0f, 0.0f), XMFLOAT2( 0, 0)},

            {XMFLOAT3(-0.5f,-0.5f, -0.5f), XMFLOAT3( 0.0f, 1.0f, 0.0f), XMFLOAT2( 0, 1)},
			{XMFLOAT3( 0.5f,-0.5f, -0.5f), XMFLOAT3( 0.0f, 1.0f, 0.0f), XMFLOAT2( 1, 1)},
			{XMFLOAT3( 0.5f,-0.5f,  0.5f), XMFLOAT3( 0.0f, 1.0f, 0.0f), XMFLOAT2( 1, 0)},
			{XMFLOAT3(-0.5f,-0.5f,  0.5f), XMFLOAT3( 0.0f, 1.0f, 0.0f), XMFLOAT2( 0, 0)},

            {XMFLOAT3( 0.5f,-0.5f, -0.5f), XMFLOAT3(-1.0f, 0.0f, 0.0f), XMFLOAT2( 0, 1)},
			{XMFLOAT3( 0.5f, 0.5f, -0.5f), XMFLOAT3(-1.0f, 0.0f, 0.0f), XMFLOAT2( 1, 1)},
			{XMFLOAT3( 0.5f, 0.5f,  0.5f), XMFLOAT3(-1.0f, 0.0f, 0.0f), XMFLOAT2( 1, 0)},
			{XMFLOAT3( 0.5f,-0.5f,  0.5f), XMFLOAT3(-1.0f, 0.0f, 0.0f), XMFLOAT2( 0, 0)},

            {XMFLOAT3(-0.5f,-0.5f, -0.5f), XMFLOAT3( 1.0f, 0.0f, 0.0f), XMFLOAT2( 0, 1)},
			{XMFLOAT3(-0.5f, 0.5f, -0.5f), XMFLOAT3( 1.0f, 0.0f, 0.0f), XMFLOAT2( 1, 1)},
			{XMFLOAT3(-0.5f, 0.5f,  0.5f), XMFLOAT3( 1.0f, 0.0f, 0.0f), XMFLOAT2( 1, 0)},
			{XMFLOAT3(-0.5f,-0.5f,  0.5f), XMFLOAT3( 1.0f, 0.0f, 0.0f), XMFLOAT2( 0, 0)},


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

			0x04 + 0,0x04 + 1,0x04 + 2,
			0x04 + 2,0x04 + 3,0x04 + 0,

			0x08 + 0,0x08 + 1,0x08 + 2,
			0x08 + 2,0x08 + 3,0x08 + 0,

			0x0C + 2,0x0C + 1,0x0C + 0,
			0x0C + 0,0x0C + 3,0x0C + 2,

			0x10 + 2,0x10 + 1,0x10 + 0,
			0x10 + 0,0x10 + 3,0x10 + 2,

			0x14 + 0,0x14 + 1,0x14 + 2,
			0x14 + 2,0x14 + 3,0x14 + 0,

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
			)
			);

        auto av = concurrency::direct3d::create_accelerator_view (m_deviceResources->GetD3DDevice());
        m_av = std::make_shared<accelerator_view> (av);
	});

	// Once the cube is loaded, the object is ready to be rendered.
	createCubeTask.then([this] () {
		m_loadingComplete = true;
	});
}

void Sample3DSceneRenderer::ReleaseDeviceDependentResources()
{
	m_loadingComplete = false;
    m_av.reset();
	m_vertexShader.Reset();
	m_inputLayout.Reset();
	m_pixelShader.Reset();
	m_constantBuffer.Reset();
	m_vertexBuffer.Reset();
	m_indexBuffer.Reset();
}