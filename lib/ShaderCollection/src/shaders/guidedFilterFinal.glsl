#version 450
layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(set = 0, binding = 1, r32f) uniform readonly image2D Amean;
layout(set = 0, binding = 2, r32f) uniform readonly image2D Bmean;
layout(set = 0, binding = 0, r32f) uniform readonly image2D I;
layout(set = 0, binding = 3, r32f) uniform writeonly image2D _output;

void main() {
  float _64 = floor((((imageLoad(Amean, ivec2(int(gl_GlobalInvocationID.x) - 10, int(gl_GlobalInvocationID.y) - 10)).x +
                       imageLoad(Amean, ivec2(int(gl_GlobalInvocationID.x) + 10, int(gl_GlobalInvocationID.y) + 10)).x) -
                      imageLoad(Amean, ivec2(int(gl_GlobalInvocationID.x) - 10, int(gl_GlobalInvocationID.y) + 10)).x) -
                     imageLoad(Amean, ivec2(int(gl_GlobalInvocationID.x) + 10, int(gl_GlobalInvocationID.y) - 10)).x) /
                    441.0);
  float _110 = floor((((imageLoad(Bmean, ivec2(int(gl_GlobalInvocationID.x) - 10, int(gl_GlobalInvocationID.y) - 10)).x +
                        imageLoad(Bmean, ivec2(int(gl_GlobalInvocationID.x) + 10, int(gl_GlobalInvocationID.y) + 10)).x) -
                       imageLoad(Bmean, ivec2(int(gl_GlobalInvocationID.x) - 10, int(gl_GlobalInvocationID.y) + 10)).x) -
                      imageLoad(Bmean, ivec2(int(gl_GlobalInvocationID.x) + 10, int(gl_GlobalInvocationID.y) - 10)).x) /
                     441.0);
  vec4 _118 = imageLoad(I, ivec2(int(gl_GlobalInvocationID.x), int(gl_GlobalInvocationID.y)));
  imageStore(_output, ivec2(int(gl_GlobalInvocationID.x), int(gl_GlobalInvocationID.y)),
             vec4((_64 * _118.x) + _110, (_64 * _118.x) + _110, (_64 * _118.x) + _110, (_64 * _118.x) + _110) / 256);
}
