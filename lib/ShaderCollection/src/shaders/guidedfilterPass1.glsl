#version 450
#extension GL_KHR_shader_subgroup_basic : require
#extension GL_KHR_shader_subgroup_arithmetic : require
layout(local_size_x = 1, local_size_y = 16, local_size_z = 1) in;

layout(set = 0, binding = 1, r32f) uniform writeonly image2D columnReducedMatrix;
layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 3, r32f) uniform writeonly image2D rowReducedMatrix;
layout(set = 0, binding = 2, r32f) uniform writeonly image2D squaredColumnReducedMatrix;
layout(set = 0, binding = 4, r32f) uniform writeonly image2D squaredRowReducedMatrix;

vec2 _10[16];

void main() {
  vec2 _15[16] = _10;
  uint _70;
  for (uint _71 = 0u; _71 < 16u; _71 = _70) {
    float _39 = imageLoad(_input, ivec2(int((16u * gl_WorkGroupID.x) + _71), int((16u * gl_WorkGroupID.y) + gl_SubgroupInvocationID))).x;
    vec2 _41 = vec2(_39, _39 * _39);
    _15[_71] = _41;
    vec2 _45 = subgroupAdd(_41);
    if (gl_SubgroupInvocationID == 0u) {
      imageStore(columnReducedMatrix, ivec2(uvec2(gl_WorkGroupID.x, (16u * gl_WorkGroupID.y) + _71)), vec4(_45.x));
      imageStore(squaredColumnReducedMatrix, ivec2(uvec2(gl_WorkGroupID.x, (16u * gl_WorkGroupID.y) + _71)), vec4(_45.y));
    } else {
    }
    _70 = _71 + 1u;
  }
  vec2 _83;
  _83 = vec2(0.0);
  vec2 _81;
  uint _82;
  for (uint _84 = 0u; _84 < 16u; _83 = _81, _84 = _82) {
    _81 = _15[_84] + _83;
    _82 = _84 + 1u;
  }
  imageStore(rowReducedMatrix, ivec2(uvec2((16u * gl_WorkGroupID.x) + gl_SubgroupInvocationID, gl_WorkGroupID.y)), vec4(_83.x));
  imageStore(squaredRowReducedMatrix, ivec2(uvec2((16u * gl_WorkGroupID.x) + gl_SubgroupInvocationID, gl_WorkGroupID.y)), vec4(_83.y));
}
