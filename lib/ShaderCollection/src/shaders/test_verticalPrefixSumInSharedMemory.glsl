#version 450
layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(set = 1, binding = 0, std140) uniform ubo
{
    int line;
} ubo_1;

layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 1, r32f) uniform image2D _output;

shared float sharedfloat[1024];

void main()
{
    sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] = imageLoad(_input, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x), (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y))).x;
    sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] = imageLoad(_input, ivec2(((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x)) + 16, (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y))).x;
    barrier();
    if (gl_LocalInvocationID.x < gl_LocalInvocationID.y)
    {
        float _95 = sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)];
        sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] = sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)];
        sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)] = _95;
    }
    else
    {
    }
    if ((gl_LocalInvocationID.x + 16u) < gl_LocalInvocationID.y)
    {
        float _126 = sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)];
        sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] = sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))];
        sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))] = _126;
    }
    else
    {
    }
    barrier();
    int _189;
    int _190;
    uint _191;
    _189 = 1;
    _190 = int((2u * gl_LocalInvocationID.x) + 1u);
    _191 = 0u;
    for (;;)
    {
        if (_191 < 5u)
        {
            if (((_190 + _189) - 1) < 32)
            {
                sharedfloat[((_190 + _189) - 1) + (32 * int(gl_LocalInvocationID.y))] = sharedfloat[(_190 - 1) + (32 * int(gl_LocalInvocationID.y))] + sharedfloat[((_190 + _189) - 1) + (32 * int(gl_LocalInvocationID.y))];
            }
            else
            {
            }
            barrier();
            _189 = 2 * _189;
            _190 = 2 * _190;
            _191 = 1u + _191;
            continue;
        }
        else
        {
            break;
        }
    }
    if (gl_LocalInvocationID.x == 0u)
    {
        sharedfloat[31u + (32u * gl_LocalInvocationID.y)] = sharedfloat[0u + (32u * gl_LocalInvocationID.y)] + imageLoad(_output, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.y), (32 * (ubo_1.line - int(gl_WorkGroupID.x))) - 1)).x;
    }
    else
    {
    }
    int _278;
    int _279;
    uint _280;
    _278 = 16;
    _279 = int((32u * gl_LocalInvocationID.x) + 16u);
    _280 = 0u;
    for (;;)
    {
        if (_280 < 6u)
        {
            if (((_279 + _278) - 1) < 32)
            {
                int _251 = ((_279 + _278) - 1) + (32 * int(gl_LocalInvocationID.y));
                float _253 = sharedfloat[_251];
                sharedfloat[((_279 + _278) - 1) + (32 * int(gl_LocalInvocationID.y))] = _253 + sharedfloat[(_279 - 1) + (32 * int(gl_LocalInvocationID.y))];
                sharedfloat[(_279 - 1) + (32 * int(gl_LocalInvocationID.y))] = _253;
            }
            else
            {
            }
            barrier();
            _278 /= 2;
            _279 /= 2;
            _280 = 1u + _280;
            continue;
        }
        else
        {
            break;
        }
    }
    if (gl_LocalInvocationID.x < gl_LocalInvocationID.y)
    {
        float _295 = sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)];
        sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] = sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)];
        sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)] = _295;
    }
    else
    {
    }
    if ((gl_LocalInvocationID.x + 16u) < gl_LocalInvocationID.y)
    {
        float _326 = sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)];
        sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] = sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))];
        sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))] = _326;
    }
    else
    {
    }
    barrier();
    imageStore(_output, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x), (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y)), vec4(sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)]));
    imageStore(_output, ivec2(((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x)) + 16, (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y)), vec4(sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)]));
}

