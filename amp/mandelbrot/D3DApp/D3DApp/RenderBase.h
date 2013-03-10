#pragma once

#include "Direct3DBase.h"

struct ModelViewProjectionConstantBuffer
{
	DirectX::XMFLOAT4X4 model;
	DirectX::XMFLOAT4X4 view;
	DirectX::XMFLOAT4X4 projection;
};

struct VertexPositionColor
{
	DirectX::XMFLOAT3 pos;
	DirectX::XMFLOAT3 color;
};

struct VertexTexPosition
{
	DirectX::XMFLOAT3 pos   ;
	DirectX::XMFLOAT3 texpos;
};

ref class RenderBase abstract : public Direct3DBase
{
protected private:
    RenderBase(void);
public:
    virtual ~RenderBase(void);

	virtual void Update(float timeTotal, float timeDelta);

    virtual void Clear ();

};

