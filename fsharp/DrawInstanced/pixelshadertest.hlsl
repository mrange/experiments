struct VSInput
{
    float4 position     : POSITION0     ;
    float4 normal       : NORMAL0       ;
    float4 color        : COLOR0        ;
    float4 iposition    : TEXCOORD0     ;
    float4 icolor       : COLOR1        ;
    uint   instanceId   : SV_INSTANCEID ;
};

struct PSInput
{
    float4 position     : SV_POSITION   ;
    float4 color        : COLOR         ;
};

cbuffer ConstantBuffer
{
    float4              viewPos         = 0.0;
    float4              lightningPos    = 0.0;
    row_major float4x4  world           = 0.0;
    row_major float4x4  worldViewProj   = 0.0;
    float4              timestamp       = 0.0;
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
    float4      pos = 1;
    pos.xyz         = input.position.xyz + input.iposition.xyz;
    result.position = mul (pos, worldViewProj);

    float4      col = input.color * input.icolor;

    float4      amb = 0.75;
    float4      acol= amb*col;

    float4      wpos= mul (pos, world);
    float4      wnor= mul (input.normal, world);
    float4      ldir= normalize (lightningPos - wpos);

    float       ddot= dot (ldir, wnor);

    if (ddot > 0)
    {
        float4  lref    = reflect (ldir, wnor);
        float4  vdir    = normalize (wpos - viewPos);
        float   sdot    = max (dot (lref, vdir), 0);

        float   spec    = pow (sdot, 10);
        result.color    = saturate (spec + (1.0 - amb)*ddot*col + acol);
    }
    else
    {
        result.color    = acol;
    }

    return result;
}

float4 PSMain (PSInput input) : SV_TARGET
{
    return input.color;
}
