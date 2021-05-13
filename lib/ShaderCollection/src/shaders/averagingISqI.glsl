#version 450

layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(set = 0, binding = 0, r32f) uniform readonly image2D I;
layout(set = 0, binding = 1, r32f) uniform readonly image2D squaredI;
layout(set = 0, binding = 2, r32f) uniform writeonly image2D meanA;
layout(set = 0, binding = 3, r32f) uniform writeonly image2D meanB;

vec2 getAandB(uint i, uint j) {
  float _100 = (imageLoad(I, ivec2(i - 10, j - 10)).x +
                   imageLoad(I, ivec2(i + 10, j + 10)).x -
                  imageLoad(I, ivec2(i - 10, j + 10)).x -
                 imageLoad(I, ivec2(i + 10, j - 10)).x) /
                441.0;
  float _178 = (imageLoad(squaredI, ivec2(i - 10, j - 10)).x +
                   imageLoad(squaredI, ivec2(i + 10, j + 10)).x -
                  imageLoad(squaredI, ivec2(i - 10, j + 10)).x -
                 imageLoad(squaredI, ivec2(i + 10, j - 10)).x) /
                441.0;
  float _186 = floor((_178 - (_100 * _100)) / ((_178 - (_100 * _100)) + 0.00999999977648258209228515625));
  return vec2(_186, _100 * (1.0 - _186));
}

void main() {
  vec2 IsqI = getAandB(int(gl_GlobalInvocationID.y), int(gl_GlobalInvocationID.x));
  imageStore(meanA, ivec2(int(gl_GlobalInvocationID.y), int(gl_GlobalInvocationID.x)), IsqI.xxxx);
  imageStore(meanB, ivec2(int(gl_GlobalInvocationID.y), int(gl_GlobalInvocationID.x)), IsqI.yyyy);
}
