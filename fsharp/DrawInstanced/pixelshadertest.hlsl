struct VSInput
{
    float4 position     : POSITION0     ;
    float4 normal       : NORMAL0       ;
    float4 color        : COLOR0        ;
    float2 texpos       : TEXCOORD0     ;
    float4 iposition    : TEXCOORD1     ;
    float4 icolor       : COLOR1        ;
    uint   instanceId   : SV_INSTANCEID ;
};

struct PSInput
{
    float4 position     : SV_POSITION0  ;
    float4 color        : COLOR0        ;
    float2 texpos       : TEXCOORD0     ;
    float4 normal       : NORMAL0       ;
    float4 pos          : POSITION0     ;
    float4 lightPos     : TEXCOORD1     ;
    float4 viewPos      : TEXCOORD2     ;
};

cbuffer ConstantBuffer
{
    float4              viewPos         = 0.0;
    float4              lightningPos    = 0.0;
    row_major float4x4  world           = 0.0;
    row_major float4x4  worldViewProj   = 0.0;
    float4              timestamp       = 0.0;
};

PSInput VSMain (VSInput input)
{
    PSInput result;
    float4      pos       = 1;
    pos.xyz               = input.position.xyz + input.iposition.xyz;

    float4      wpos      = mul (pos, world);
    float4      wnor      = mul (input.normal, world);

    result.position       = mul (pos, worldViewProj);
    result.color          = input.color * input.icolor;
    result.texpos         = input.texpos;
    result.pos            = wpos;
    result.normal         = wnor;
    result.lightPos       = lightningPos;
    result.viewPos        = viewPos;

    return result;
}

float4 PSMain (PSInput input) : SV_TARGET
{
    float2 tp     = input.texpos;
    float4 mod    = {tp.x, tp.y, 1.0, 1.0};
    float4 col    = mod*input.color;

    float4 result ;

    float4 wpos   = input.pos       ;
    float4 wnor   = input.normal    ;
    float4 lpos   = input.lightPos  ;
    float4 vpos   = input.viewPos   ;

    float4 ldir   = normalize (lpos - wpos);
    float  ddot   = dot (ldir, wnor);

    float4 amb    = 0.75;
    float4 acol   = amb*col;

    if (ddot > 0)
    {
        float4  lref    = reflect (ldir, wnor);
        float4  vdir    = normalize (wpos - vpos);
        float   sdot    = max (dot (lref, vdir), 0);

        float   spec    = pow (sdot, 10);
        result          = saturate (spec + (1.0 - amb)*ddot*col + acol);
    }
    else
    {
        result          = acol;
    }

    return result;
}
