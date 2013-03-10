#pragma once

#include "RenderBase.h"

// This class renders a simple spinning cube.
ref class CubeRenderer sealed : public RenderBase
{
public:
	CubeRenderer();

	// Direct3DBase methods.
	virtual void CreateDeviceResources() override;
	virtual void CreateWindowSizeDependentResources() override;
	virtual void Render() override;
	
	// Method for updating time-dependent objects.
	virtual void Update(float timeTotal, float timeDelta) override;

private:
	bool m_loadingComplete;

	Microsoft::WRL::ComPtr<ID3D11InputLayout>   m_inputLayout;
	Microsoft::WRL::ComPtr<ID3D11Buffer>        m_vertexBuffer;
	Microsoft::WRL::ComPtr<ID3D11Buffer>        m_indexBuffer;
	Microsoft::WRL::ComPtr<ID3D11VertexShader>  m_vertexShader;
	Microsoft::WRL::ComPtr<ID3D11PixelShader>   m_pixelShader;
	Microsoft::WRL::ComPtr<ID3D11Buffer>        m_constantBuffer;

	uint32 m_indexCount;
	ModelViewProjectionConstantBuffer m_constantBufferData;
};
