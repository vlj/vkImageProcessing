#version 450
#extension GL_KHR_shader_subgroup_basic : require
#extension GL_KHR_shader_subgroup_arithmetic : require
layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;

layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 1, r32f) uniform writeonly image2D _output;

void main() {
  uint _16 = (gl_WorkGroupID.x * 32u) + gl_SubgroupID;
  uvec2 _25 = uvec2(imageSize(_input));
  float _44;
  uint _45;
  float _46 = 0.0;
  uint _47 = 0u;
  for (; _47 < _25.x; _46 = _44, _47 = _45) {
    float _35 = imageLoad(_input, ivec2(uvec2(_47 + gl_SubgroupInvocationID, _16))).x;
    imageStore(_output, ivec2(uvec2(_47 + gl_SubgroupInvocationID, _16)), vec4(_46 + subgroupInclusiveAdd(_35)));
    _44 = subgroupAdd(_35) + _46;
    _45 = _47 + 32u;
  }
}
