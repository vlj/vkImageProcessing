#version 450
#define kSubgroupSize 16
layout(local_size_x = 1, local_size_y = kSubgroupSize, local_size_z = 1) in;

#extension GL_KHR_shader_subgroup_arithmetic : enable



layout(set = 0, binding = 4, r32f) uniform writeonly image2D AReducedMatrix;
layout(set = 0, binding = 2, r32f) uniform writeonly image2D AcolumnReducedMatrix;
layout(set = 0, binding = 3, r32f) uniform writeonly image2D BColumnReducedMatrix;
layout(set = 0, binding = 5, r32f) uniform writeonly image2D BRowReducedMatrix;
layout(set = 0, binding = 0, r32f) uniform readonly image2D I;
layout(set = 0, binding = 1, r32f) uniform readonly image2D squaredI;


vec2 getAandB(uint i, uint j) {
  return vec2(
      imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i), ((kSubgroupSize * int(gl_WorkGroupID.y)) + j))).x,
      imageLoad(squaredI, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10)).x);
  /*
  float _100 = floor(
      (((imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10)).x +
         imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) + 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) + 10)).x) -
        imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) + 10)).x) -
       imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) + 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10)).x) /
                     441.0);
  float _178 = floor(
      ((( +
         imageLoad(squaredI, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) + 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) + 10))
             .x) -
        imageLoad(squaredI, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) + 10))
            .x) -
       imageLoad(squaredI, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) + 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10))
           .x) /
                     441.0);
  float _186 = floor((_178 - (_100 * _100)) / ((_178 - (_100 * _100)) + 0.00999999977648258209228515625));
  return vec2(_186, _100 * (1.0 - _186));*/
}

void main() {

  vec2 tmp[kSubgroupSize];

  for (int i = 0; i < kSubgroupSize; i++) {
    vec2 currentISqILine = getAandB(i, gl_SubgroupInvocationID);
    vec2 rowsum = subgroupAdd(currentISqILine);
    if (gl_SubgroupInvocationID == 0) {
      ivec2 coord = ivec2(gl_WorkGroupID.x, (kSubgroupSize * gl_WorkGroupID.y) + i);
      imageStore(AcolumnReducedMatrix, coord, vec4(rowsum.xxxx));
      imageStore(BColumnReducedMatrix, coord, vec4(rowsum.yyyy));
    }
    tmp[i] = currentISqILine;
  }

  vec2 acc = vec2(0);
  for (int i = 0; i < kSubgroupSize; i++) {
    acc += tmp[i];
  }

  ivec2 coord = ivec2((kSubgroupSize * gl_WorkGroupID.x) + gl_SubgroupInvocationID, gl_WorkGroupID.y);
  imageStore(AReducedMatrix, coord, vec4(acc.xxxx));
  imageStore(BRowReducedMatrix, coord, vec4(acc.yyyy));

}
