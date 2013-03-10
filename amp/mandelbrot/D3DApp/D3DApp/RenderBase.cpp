#include "pch.h"
#include "RenderBase.h"


RenderBase::RenderBase(void)
{
}


RenderBase::~RenderBase(void)
{
}

void RenderBase::Update(float timeTotal, float timeDelta)
{
}

void RenderBase::Clear ()
{
	const float midnightBlue[] = { 0.098f, 0.098f, 0.439f, 1.000f };
	m_d3dContext->ClearRenderTargetView(
		m_renderTargetView.Get(),
		midnightBlue
		);

	m_d3dContext->ClearDepthStencilView(
		m_depthStencilView.Get(),
		D3D11_CLEAR_DEPTH,
		1.0f,
		0
		);
}