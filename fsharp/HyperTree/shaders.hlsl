struct VSInput
{
	float2 position		: TEXCOORD0		;

	float4 icolor		: COLOR1		;
	float2 iposition	: TEXCOORD1		;
	float2 isize		: TEXCOORD2		;

	uint   instanceId	: SV_INSTANCEID	;
};

struct PSInput
{
	float4 position		: SV_POSITION	;
	float4 color		: COLOR			;
};

cbuffer ConstantBuffer
{
	float4				offset			= 0.0;
	row_major float4x4	world			= 0.0;
	row_major float4x4	worldViewProj	= 0.0;
	float4				timestamp		= 0.0;
};

float4x4 rotx (float a)
{
	float4x4 m =
	{
		1,      0,       0, 0,
		0, cos(a), -sin(a), 0,
		0, sin(a),  cos(a), 0,
		0,      0,       0, 1,
	};
	return m;
}

float4x4 roty (float a)
{
	float4x4 m =
	{
		cos(a), 0, -sin(a), 0,
		     0, 1,       0, 0,
		sin(a), 0,  cos(a), 0,
		     0, 0,       0, 1,
	};
	return m;
}

float4x4 rotz (float a)
{
	float4x4 m =
	{
		cos(a), -sin(a), 0, 0,
		sin(a),  cos(a), 0, 0,
		     0,       0, 1, 0,
		     0,       0, 0, 1,
	};
	return m;
}

float2 transform (float2 v)
{
	// Based on: https://en.wikipedia.org/wiki/Poincar%C3%A9_disk_model
	//	https://en.wikipedia.org/wiki/Poincar%C3%A9_disk_model#/media/File:HyperboloidProjection.png
	// TODO: Simplify this
	// t^2 = x^2 + y^2 + 1
	float3	eye	= { 0.F, 0.F, -1.F };

	float	t	= sqrt (dot (v, v) + 1.F);
	float3	h	= {v.x, v.y, t};
	float3	d	= h - eye;

	float3	r	= (1.F / (t + 1.F)) * d + eye;

	return r.xy;
}

PSInput VSMain (VSInput input)
{
	PSInput result;

	float2		v	= input.isize.xy*input.position.xy + input.iposition.xy + offset.xy;
	float2		tv	= transform(v);

	float4		col	= input.icolor;
	float4		pos	= { 0, 0, 0, 1 };
	
	pos.xy = tv;

	result.position = mul (pos, worldViewProj);
	result.color	= col;

	return result;
}

float4 PSMain (PSInput input) : SV_TARGET
{
	return input.color;
}