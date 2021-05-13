#version 450
#define kSubgroupSize 16
layout(local_size_x = 1, local_size_y = kSubgroupSize, local_size_z = 1) in;

#extension GL_KHR_shader_subgroup_arithmetic : enable

layout(set = 0, binding = 4, r32f) uniform readonly image2D AReducedMatrix;
layout(set = 0, binding = 2, r32f) uniform readonly image2D AcolumnReducedMatrix;
layout(set = 0, binding = 6, r32f) uniform writeonly image2D Amean;
layout(set = 0, binding = 3, r32f) uniform readonly image2D BColumnReducedMatrix;
layout(set = 0, binding = 5, r32f) uniform readonly image2D BRowReducedMatrix;
layout(set = 0, binding = 7, r32f) uniform writeonly image2D Bmean;
layout(set = 0, binding = 0, r32f) uniform readonly image2D I;
layout(set = 0, binding = 1, r32f) uniform readonly image2D squaredI;

vec2 getAandB(uint i, uint j) {
    return vec2(
        imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i), ((kSubgroupSize * int(gl_WorkGroupID.y)) + j))).x,
        imageLoad(squaredI, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10))
            .x);
/*  float _100 = floor(
      (((imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10)).x +
         imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) + 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) + 10)).x) -
        imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) + 10)).x) -
       imageLoad(I, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) + 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10)).x) /
      441.0);
  float _178 = floor(
      (((imageLoad(squaredI, ivec2(((kSubgroupSize * int(gl_WorkGroupID.x)) + i) - 10, ((kSubgroupSize * int(gl_WorkGroupID.y)) + j) - 10))
             .x +
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

shared vec2 sharedVec2[1056];

void main() {


    
  vec2 tmp[32];
  for (int i = 0; i < kSubgroupSize; i++) {
    vec2 ISqI = getAandB(i, gl_SubgroupInvocationID);
    vec2 rowsum = subgroupInclusiveAdd(ISqI);
    rowsum += vec2(
        imageLoad(AcolumnReducedMatrix, ivec2(int(gl_WorkGroupID.x) - 1, (kSubgroupSize * int(gl_WorkGroupID.y)) + int(i))).x,
                   imageLoad(BColumnReducedMatrix, ivec2(int(gl_WorkGroupID.x) - 1, (kSubgroupSize * int(gl_WorkGroupID.y)) + int(i))).x);
    tmp[i] = rowsum;
  }

  vec2 colsum2 = vec2(0);
  for (int i = 0; i < kSubgroupSize; i++) {
    colsum2 += tmp[i];
    tmp[i] = colsum2;

    tmp[i] += vec2(
        imageLoad(AReducedMatrix,
                             ivec2((kSubgroupSize * int(gl_WorkGroupID.x)) + int(gl_SubgroupInvocationID), int(gl_WorkGroupID.y) - 1))
                       .x,
        imageLoad(BRowReducedMatrix,
                             ivec2((kSubgroupSize * int(gl_WorkGroupID.x)) + int(gl_SubgroupInvocationID), int(gl_WorkGroupID.y) - 1))
                       .x);

    imageStore(Amean,
               ivec2((kSubgroupSize * int(gl_WorkGroupID.x)) + gl_SubgroupInvocationID, (kSubgroupSize * int(gl_WorkGroupID.y)) + int(i)),
               vec4(tmp[i].xxxx));

    imageStore(Bmean,
               ivec2((kSubgroupSize * int(gl_WorkGroupID.x)) + gl_SubgroupInvocationID, (kSubgroupSize * int(gl_WorkGroupID.y)) + int(i)),
               vec4(tmp[i].yyyy));
  }
}
