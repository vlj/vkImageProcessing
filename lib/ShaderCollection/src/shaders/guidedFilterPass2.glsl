#version 450
#extension GL_KHR_shader_subgroup_basic : require
#extension GL_KHR_shader_subgroup_arithmetic : require
layout(local_size_x = 8, local_size_y = 16, local_size_z = 1) in;

layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 5, r32f) uniform writeonly image2D outputMean;
layout(set = 0, binding = 6, r32f) uniform writeonly image2D outputSquaredMean;
layout(set = 0, binding = 1, r32f) uniform readonly image2D summedColumnReducedMatrix;
layout(set = 0, binding = 3, r32f) uniform readonly image2D summedRowReducedMatrix;
layout(set = 0, binding = 2, r32f) uniform readonly image2D summedSquaredColumnReducedMatrix;
layout(set = 0, binding = 4, r32f) uniform readonly image2D summedSquaredRowReducedMatrix;

vec2 _10[16];

void main() {
  vec2 _15[16] = _10;
  uint _73;
  for (uint _74 = 0u; _74 < 16u; _74 = _73) {
    float _39 = imageLoad(_input, ivec2(int((16u * gl_WorkGroupID.x) + gl_SubgroupInvocationID), int((16u * gl_WorkGroupID.y) + _74))).x;
    vec2 _41 = vec2(_39, _39 * _39);
    _15[_74] = _41;
    _15[_74] = vec2(imageLoad(summedColumnReducedMatrix, ivec2(int(gl_WorkGroupID.x) - 1, int((16u * gl_WorkGroupID.y) + _74))).x,
                    imageLoad(summedSquaredColumnReducedMatrix, ivec2(int(gl_WorkGroupID.x) - 1, int((16u * gl_WorkGroupID.y) + _74))).x) +
               subgroupInclusiveAdd(_41);
    _73 = _74 + 1u;
  }
  vec2 _143;
  _143 = vec2(0.0);
  vec2 _85;
  uint _142;
  for (uint _144 = 0u; _144 < 16u; _143 = _85, _144 = _142) {
    _85 = _15[_144] + _143;
    _15[_144] =
        _85 +
        vec2(imageLoad(summedRowReducedMatrix, ivec2(int((gl_WorkGroupID.x * 16u) + gl_SubgroupInvocationID), int(gl_WorkGroupID.y) - 1)).x,
             imageLoad(summedSquaredRowReducedMatrix,
                       ivec2(int((gl_WorkGroupID.x * 16u) + gl_SubgroupInvocationID), int(gl_WorkGroupID.y) - 1))
                 .x);
    imageStore(outputMean, ivec2((16 * int(gl_WorkGroupID.x)) + int(gl_SubgroupInvocationID), (16 * int(gl_WorkGroupID.y)) + int(_144)),
               vec4(_15[_144].x));
    imageStore(outputSquaredMean,
               ivec2((16 * int(gl_WorkGroupID.x)) + int(gl_SubgroupInvocationID), (16 * int(gl_WorkGroupID.y)) + int(_144)),
               vec4(_15[_144].y));
    _142 = _144 + 1u;
  }
}
