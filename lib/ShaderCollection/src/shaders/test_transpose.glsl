#version 450
layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(binding = 0, set=1, std140) uniform ubo
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
    imageStore(_output, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x), (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y)), vec4(sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)]));
    imageStore(_output, ivec2(((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x)) + 16, (32 * (ubo_1.line - int(gl_WorkGroupID.x))) + int(gl_LocalInvocationID.y)), vec4(sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)]));
}

