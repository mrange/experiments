struct VSInput
{
	float4 position		: POSITION0		;
	float4 color		: COLOR0		;
	float4 iposition	: TEXCOORD1		;
	float4 idirection	: TEXCOORD2		;
	float4 irotation	: TEXCOORD3		;
	float4 idelay		: TEXCOORD4		;
	uint   instanceId	: SV_INSTANCEID	;
};

struct PSInput
{
	float4 position		: SV_POSITION	;
	float4 color		: COLOR			;
};

cbuffer ConstantBuffer
{
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

PSInput VSMain (VSInput input)
{
	PSInput result;
	float		ts	= max (0, input.idelay.x - timestamp.x);
	float		rts	= sqrt(ts);
	float		dts	= ts*ts;
	float4x4	rot	= mul (rotx (rts*input.irotation.x), mul (roty (rts*input.irotation.y), rotz (rts*input.irotation.z)));
	float4		dir	= mul (rot, input.idirection);
	float4		box = mul (rot, input.position);
	float4		pos	= 1;
	pos.xyz			= box.xyz + input.iposition.xyz + dts*dir.xyz;
	result.position = mul (pos, worldViewProj);
	result.color	= input.color;

	return result;
}

float4 PSMain (PSInput input) : SV_TARGET
{
	return input.color;
}