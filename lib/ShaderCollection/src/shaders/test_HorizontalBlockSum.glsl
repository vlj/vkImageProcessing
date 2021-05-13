#version 450
layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(binding = 0, std140, set = 1) uniform ubo
{
    int line;
} ubo_1;

layout(binding = 0, r32f) uniform readonly image2D _input;
layout(binding = 1, r32f) uniform writeonly image2D _output;

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
    sharedfloat[31u + (32u * gl_LocalInvocationID.y)] = 0.0;
    int _183;
    int _184;
    uint _185;
    _183 = 16;
    _184 = int((32u * gl_LocalInvocationID.x) + 16u);
    _185 = 0u;
    for (;;)
    {
        if (_185 < 6u)
        {
            if (((_184 + _183) - 1) < 32)
            {
                int _156 = ((_184 + _183) - 1) + (32 * int(gl_LocalInvocationID.y));
                float _158 = sharedfloat[_156];
                sharedfloat[((_184 + _183) - 1) + (32 * int(gl_LocalInvocationID.y))] = _158 + sharedfloat[(_184 - 1) + (32 * int(gl_LocalInvocationID.y))];
                sharedfloat[(_184 - 1) + (32 * int(gl_LocalInvocationID.y))] = _158;
            }
            else
            {
            }
            barrier();
            _183 /= 2;
            _184 /= 2;
            _185 = 1u + _185;
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

