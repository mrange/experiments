struct VSInput
{
    float4 position     : POSITION0     ;
    float4 normal       : NORMAL0       ;
    float4 tangent      : TANGENT0      ;
    float4 binormal     : BINORMAL0     ;
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
    float4 tangent      : TANGENT0      ;
    float4 binormal     : BINORMAL0     ;
    float4 pos          : POSITION0     ;
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
    float4      wtng      = mul (input.tangent, world);
    float4      wbi       = mul (input.binormal, world);

    result.position       = mul (pos, worldViewProj);
    result.color          = input.color * input.icolor;
    result.texpos         = input.texpos;
    result.pos            = wpos;
    result.normal         = wnor;
    result.tangent        = wtng;
    result.binormal       = wbi;

    return result;
}

float4 PSMain (PSInput input) : SV_TARGET
{
    float2 mid    = {0.5, 0.5};
    float2 tp     = 2.0*(input.texpos - mid);
    float  dist   = 4.0*length(tp);
    float  angle  = dist*dist*dist + timestamp;
    float  inten  = 0.25*cos (angle) + 0.75;
    float4 cmod   = {inten, inten, 1.0, 1.0};
    float4 col    = cmod*input.color;

    float2 ntp    = normalize (tp);
    float  coln   = 0.25*sin(angle);
    float3 n      = {ntp.x*coln, ntp.y*coln, 1};
    float3 nn     = normalize(n);

    float4 result ;

    float4 wpos   = input.pos         ;
    float4 wnor   = input.normal      ;
    float4 wtng   = input.tangent     ;
    float4 wbi    = input.binormal    ;
    float4 lpos   = lightningPos      ;
    float4 vpos   = viewPos           ;

    float4 norm   = nn.x*wtng + nn.y*wbi + nn.z*wnor;

    float4 ldir   = normalize (lpos - wpos);
    float  ddot   = dot (ldir, norm);

    float4 amb    = 0.75;
    float4 acol   = amb*col;

    if (ddot > 0)
    {
        float4  lref    = reflect (ldir, norm);
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
