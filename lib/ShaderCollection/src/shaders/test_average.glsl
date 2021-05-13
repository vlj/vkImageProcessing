#version 450
layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 1, r32f) uniform writeonly image2D _output;

void main()
{
    vec4 _26 = imageLoad(_input, ivec2(int(gl_GlobalInvocationID.x) - 1, int(gl_GlobalInvocationID.y) - 1));
    vec4 _35 = imageLoad(_input, ivec2(int(gl_GlobalInvocationID.x) + 1, int(gl_GlobalInvocationID.y) - 1));
    vec4 _44 = imageLoad(_input, ivec2(int(gl_GlobalInvocationID.x) - 1, int(gl_GlobalInvocationID.y) + 1));
    vec4 _53 = imageLoad(_input, ivec2(int(gl_GlobalInvocationID.x) + 1, int(gl_GlobalInvocationID.y) + 1));
    imageStore(_output, ivec2(int(gl_GlobalInvocationID.x), int(gl_GlobalInvocationID.y)), 100 * vec4(((_26.x + _53.x) - _44.x) - _35.x, ((_26.x + _53.x) - _44.x) - _35.x, ((_26.x + _53.x) - _44.x) - _35.x, ((_26.x + _53.x) - _44.x) - _35.x) / 100);
}

