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
    int _126;
    int _127;
    uint _128;
    _126 = 1;
    _127 = int((2u * gl_LocalInvocationID.x) + 1u);
    _128 = 0u;
    for (;;)
    {
        if (_128 < 5u)
        {
            if (((_127 + _126) - 1) < 32)
            {
                sharedfloat[((_127 + _126) - 1) + (32 * int(gl_LocalInvocationID.y))] = sharedfloat[(_127 - 1) + (32 * int(gl_LocalInvocationID.y))] + sharedfloat[((_127 + _126) - 1) + (32 * int(gl_LocalInvocationID.y))];
            }
            else
            {
            }
            barrier();
            _126 = 2 * _126;
            _127 = 2 * _127;
            _128 = 1u + _128;
            continue;
        }
        else
        {
            break;
        }
    }
    if (gl_LocalInvocationID.x == 0u)
    {
        sharedfloat[31u + (32u * gl_LocalInvocationID.y)] = sharedfloat[0u + (32u * gl_LocalInvocationID.y)] + imageLoad(_output, ivec2((32 * int(gl_WorkGroupID.x)) - 1, (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y))).x;
    }
    else
    {
    }
    int _215;
    int _216;
    uint _217;
    _215 = 16;
    _216 = int((32u * gl_LocalInvocationID.x) + 16u);
    _217 = 0u;
    for (;;)
    {
        if (_217 < 6u)
        {
            if (((_216 + _215) - 1) < 32)
            {
                int _188 = ((_216 + _215) - 1) + (32 * int(gl_LocalInvocationID.y));
                float _190 = sharedfloat[_188];
                sharedfloat[((_216 + _215) - 1) + (32 * int(gl_LocalInvocationID.y))] = _190 + sharedfloat[(_216 - 1) + (32 * int(gl_LocalInvocationID.y))];
                sharedfloat[(_216 - 1) + (32 * int(gl_LocalInvocationID.y))] = _190;
            }
            else
            {
            }
            barrier();
            _215 /= 2;
            _216 /= 2;
            _217 = 1u + _217;
            continue;
        }
        else
        {
            break;
        }
    }
    imageStore(_output, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x), (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y)), vec4(sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)]));
    imageStore(_output, ivec2(((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x)) + 16, (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y)), vec4(sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)]));
}

