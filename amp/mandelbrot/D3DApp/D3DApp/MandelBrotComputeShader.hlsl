
RWTexture2D<float> tex;

[numthreads(16, 16, 1)]
void main(
    uint3 groupId           : SV_GroupID            ,
    uint3 groupThreadId     : SV_GroupThreadID      ,
    uint3 dispatchThreadId  : SV_DispatchThreadID   ,
    uint groupIndex         : SV_GroupIndex         )
{
    tex [dispatchThreadId.xy] = 1;
}
