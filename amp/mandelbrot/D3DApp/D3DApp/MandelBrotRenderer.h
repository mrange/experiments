
#pragma once

#include "RenderBase.h"

struct MandelBrotPos
{
	DirectX::XMFLOAT3 pos   ;
	DirectX::XMFLOAT3 normal;
	DirectX::XMFLOAT2 texpos;
};


ref class MandelBrotRenderer sealed : public RenderBase
{
public:
    MandelBrotRenderer(void);
    virtual ~MandelBrotRenderer(void);

    virtual void CreateDeviceResources() override;
	virtual void CreateWindowSizeDependentResources() override;
	virtual void Render() override;

	virtual void Update(float timeTotal, float timeDelta) override;

private:
    bool                                        m_loadingComplete       ;
    uint32                                      m_indexCount            ;
	ModelViewProjectionConstantBuffer           m_constantBufferData    ;

	Microsoft::WRL::ComPtr<ID3D11InputLayout>   m_inputLayout           ;
	Microsoft::WRL::ComPtr<ID3D11Buffer>        m_vertexBuffer          ;
	Microsoft::WRL::ComPtr<ID3D11Buffer>        m_indexBuffer           ;
	Microsoft::WRL::ComPtr<ID3D11VertexShader>  m_vertexShader          ;
	Microsoft::WRL::ComPtr<ID3D11PixelShader>   m_pixelShader           ;
	Microsoft::WRL::ComPtr<ID3D11ComputeShader> m_computeShader         ;
	Microsoft::WRL::ComPtr<ID3D11Buffer>        m_constantBuffer        ;

    Microsoft::WRL::ComPtr<ID3D11Texture2D>             m_texture       ;
    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView>    m_textureView   ;
    Microsoft::WRL::ComPtr<ID3D11SamplerState>          m_sampler       ;

}
;

