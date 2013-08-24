#include "pch.h"
#include "MandelBrotRenderer.h"

using namespace concurrency;
using namespace concurrency::graphics;

using namespace DirectX;
using namespace Microsoft::WRL;
using namespace Windows::Foundation;
using namespace Windows::UI::Core;
namespace
{
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
            ID3D11Device    *   device
        ,   ID3D11Texture2D *   texture
        ,   int                 offset
        ,   float               zoom
        )
    {
        try
        {
            auto av = concurrency::direct3d::create_accelerator_view(device);

            unsigned int const  iter    = 128                   ;

            float        const  cx      = 0.001643721971153F    ;
            float        const  cy      = 0.822467633298876F    ;

            float        const  dx      = zoom                  ;
            float        const  dy      = zoom                  ;

            {
                int const lookup_size   = color_lookup.size ();

                auto lookup             = array_view<unorm_4 const, 1> (color_lookup);

                auto tex                = concurrency::graphics::direct3d::make_texture<unorm_4,2>(av, texture);
                auto wotex              = texture_view<unorm_4, 2> (tex);
                auto e                  = tex.extent;

                parallel_for_each (
                        av
                    ,   e
                    ,   [=] (index<2> idx) restrict(amp)
                    {
                        auto x = cx + dx * (((mtype)idx[0]) / e[0] - 0.5);
                        auto y = cy + dy * (((mtype)idx[1]) / e[1] - 0.5);

                        auto result = mandelbrot (x,y, iter);

                        auto color = result == iter ? unorm_4 (0.0F, 0.0F, 0.0F, 1.0F) : lookup[(result + offset) % lookup_size];

                        wotex.set(idx,color); 
                    });
            }
        }
        catch (std::exception const & e)
        {
            auto what = e.what();
            OutputDebugString(L"Known exception");
        }
        catch (...)
        {
            OutputDebugString(L"Unknown exception");
        }
    }
}



MandelBrotRenderer::MandelBrotRenderer(void)
    :   m_indexCount (0)
    ,   m_loadingComplete (false)
{
}


MandelBrotRenderer::~MandelBrotRenderer(void)
{
}


void MandelBrotRenderer::CreateDeviceResources()
{
	Direct3DBase::CreateDeviceResources();

	auto loadVSTask = DX::ReadDataAsync("MandelBrotVertexShader.cso");
	auto loadPSTask = DX::ReadDataAsync("MandelBrotPixelShader.cso");
	//auto loadCSTask = DX::ReadDataAsync("MandelBrotComputeShader.cso");
	auto loadTexTask = DX::ReadDataAsync("texturedata.bin");

	auto createVSTask = loadVSTask.then([this](Platform::Array<byte>^ fileData) {
		DX::ThrowIfFailed(
			m_d3dDevice->CreateVertexShader(
 				fileData->Data,
				fileData->Length,
				nullptr,
				&m_vertexShader
				)
			);

		const D3D11_INPUT_ELEMENT_DESC vertexDesc[] = 
		{
            { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0,  0, D3D11_INPUT_PER_VERTEX_DATA, 0 },
            { "NORMAL",   0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0 },
            { "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT,    0, 24, D3D11_INPUT_PER_VERTEX_DATA, 0 },
		};

		DX::ThrowIfFailed(
			m_d3dDevice->CreateInputLayout(
				vertexDesc,
				ARRAYSIZE(vertexDesc),
				fileData->Data,
				fileData->Length,
				&m_inputLayout
				)
			);
	});

	auto createPSTask = loadPSTask.then([this](Platform::Array<byte>^ fileData) {
		DX::ThrowIfFailed(
			m_d3dDevice->CreatePixelShader(
				fileData->Data,
				fileData->Length,
				nullptr,
				&m_pixelShader
				)
			);

		CD3D11_BUFFER_DESC constantBufferDesc(sizeof(ModelViewProjectionConstantBuffer), D3D11_BIND_CONSTANT_BUFFER);
		DX::ThrowIfFailed(
			m_d3dDevice->CreateBuffer(
				&constantBufferDesc,
				nullptr,
				&m_constantBuffer
				)
			);
	});

    /*
	auto createCSTask = loadCSTask.then([this](Platform::Array<byte>^ fileData) {
		DX::ThrowIfFailed(
			m_d3dDevice->CreateComputeShader(
				fileData->Data,
				fileData->Length,
				nullptr,
				&m_computeShader
				)
			);
	});
    */
	auto createTexTask = loadTexTask.then([this](Platform::Array<byte>^ fileData) 
        {
            D3D11_SUBRESOURCE_DATA textureSubresourceData   = {};
            textureSubresourceData.pSysMem                  = fileData->Data;
            textureSubresourceData.SysMemPitch = 1024;
            textureSubresourceData.SysMemSlicePitch = 0;

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
                m_d3dDevice->CreateTexture2D(
                        &textureDesc
                    ,   NULL
                    ,   &m_texture
                    )
                );

            D3D11_SHADER_RESOURCE_VIEW_DESC textureViewDesc = {};
            textureViewDesc.Format                          = textureDesc.Format;
            textureViewDesc.ViewDimension                   = D3D11_SRV_DIMENSION_TEXTURE2D;
            textureViewDesc.Texture2D.MipLevels             = textureDesc.MipLevels;
            textureViewDesc.Texture2D.MostDetailedMip       = 0;

            DX::ThrowIfFailed(
                m_d3dDevice->CreateShaderResourceView(
                    m_texture.Get(),
                    &textureViewDesc,
                    &m_textureView
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
                m_d3dDevice->CreateSamplerState(
                    &samplerDesc,
                    &m_sampler
                    )
                );
        });

	auto createResourcesTask = (
            createPSTask 
//        &&  createCSTask
        &&  createVSTask
        &&  createTexTask
        ).then([this] () 
        {
            MandelBrotPos cubeVertices[] = 
		    {
                {XMFLOAT3(-0.5f, -0.5f,  0.0f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 1)},
			    {XMFLOAT3( 0.5f, -0.5f,  0.0f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 1)},
			    {XMFLOAT3( 0.5f,  0.5f,  0.0f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 1, 0)},
			    {XMFLOAT3(-0.5f,  0.5f,  0.0f), XMFLOAT3( 0.0f, 0.0f,-1.0f), XMFLOAT2( 0, 0)},
		    };

		    D3D11_SUBRESOURCE_DATA vertexBufferData = {0};
		    vertexBufferData.pSysMem = cubeVertices;
		    vertexBufferData.SysMemPitch = 0;
		    vertexBufferData.SysMemSlicePitch = 0;
		    CD3D11_BUFFER_DESC vertexBufferDesc(sizeof(cubeVertices), D3D11_BIND_VERTEX_BUFFER);
		    DX::ThrowIfFailed(
			    m_d3dDevice->CreateBuffer(
				    &vertexBufferDesc,
				    &vertexBufferData,
				    &m_vertexBuffer
				    )
			    );

		    unsigned short cubeIndices[] = 
		    {
			    2,1,0,
			    0,3,2,
		    };

		    m_indexCount = ARRAYSIZE(cubeIndices);

		    D3D11_SUBRESOURCE_DATA indexBufferData = {0};
		    indexBufferData.pSysMem = cubeIndices;
		    indexBufferData.SysMemPitch = 0;
		    indexBufferData.SysMemSlicePitch = 0;
		    CD3D11_BUFFER_DESC indexBufferDesc(sizeof(cubeIndices), D3D11_BIND_INDEX_BUFFER);
		    DX::ThrowIfFailed(
			    m_d3dDevice->CreateBuffer(
				    &indexBufferDesc,
				    &indexBufferData,
				    &m_indexBuffer
				    )
			    );

            m_loadingComplete = true;
	    });
}
void MandelBrotRenderer::Update(float timeTotal, float timeDelta)
{
	(void) timeDelta; // Unused parameter.

	XMVECTOR eye = XMVectorSet(0.0f, 0.0f, 1.5f, 0.0f);
	XMVECTOR at = XMVectorSet(0.0f, 00.0f, 0.0f, 0.0f);
	XMVECTOR up = XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f);

	XMStoreFloat4x4(&m_constantBufferData.view, XMMatrixTranspose(XMMatrixLookAtRH(eye, at, up)));
	XMStoreFloat4x4(&m_constantBufferData.model, XMMatrixTranspose(XMMatrixRotationY(0* XM_PIDIV4)));

    auto zoom = 2.0F / static_cast<float> (pow(1.2, timeTotal));

    compute_mandelbrot (
            m_d3dDevice.Get()
        ,   m_texture.Get()
        ,   static_cast<int> (timeTotal * 10)
        ,   zoom
        );

}


void MandelBrotRenderer::CreateWindowSizeDependentResources()
{
	Direct3DBase::CreateWindowSizeDependentResources();

	float aspectRatio = m_windowBounds.Width / m_windowBounds.Height;
	float fovAngleY = 70.0f * XM_PI / 180.0f;

	// Note that the m_orientationTransform3D matrix is post-multiplied here
	// in order to correctly orient the scene to match the display orientation.
	// This post-multiplication step is required for any draw calls that are
	// made to the swap chain render target. For draw calls to other targets,
	// this transform should not be applied.
	XMStoreFloat4x4(
		&m_constantBufferData.projection,
		XMMatrixTranspose(
			XMMatrixMultiply(
				XMMatrixPerspectiveFovRH(
					fovAngleY,
					aspectRatio,
					0.01f,
					100.0f
					),
				XMLoadFloat4x4(&m_orientationTransform3D)
				)
			)
		);
}

void MandelBrotRenderer::Render() 
{
	// Only draw the cube once it is loaded (loading is asynchronous).
	if (!m_loadingComplete)
	{
		return;
	}

	m_d3dContext->OMSetRenderTargets(
		1,
		m_renderTargetView.GetAddressOf(),
		m_depthStencilView.Get()
		);

	m_d3dContext->UpdateSubresource(
		m_constantBuffer.Get(),
		0,
		NULL,
		&m_constantBufferData,
		0,
		0
		);

	UINT stride = sizeof(MandelBrotPos);
	UINT offset = 0;
	m_d3dContext->IASetVertexBuffers(
		0,
		1,
		m_vertexBuffer.GetAddressOf(),
		&stride,
		&offset
		);

	m_d3dContext->IASetIndexBuffer(
		m_indexBuffer.Get(),
		DXGI_FORMAT_R16_UINT,
		0
		);

	m_d3dContext->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

	m_d3dContext->IASetInputLayout(m_inputLayout.Get());

	m_d3dContext->VSSetShader(
		m_vertexShader.Get(),
		nullptr,
		0
		);

	m_d3dContext->VSSetConstantBuffers(
		0,
		1,
		m_constantBuffer.GetAddressOf()
		);

	m_d3dContext->PSSetShader(
		m_pixelShader.Get(),
		nullptr,
		0
		);

        m_d3dContext->PSSetShaderResources(
            0,
            1,
            m_textureView.GetAddressOf()
            );

        m_d3dContext->PSSetSamplers(
            0,
            1,
            m_sampler.GetAddressOf()
            );

	m_d3dContext->DrawIndexed(
		m_indexCount,
		0,
		0
		);
}
