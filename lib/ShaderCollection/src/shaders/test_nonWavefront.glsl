#version 450
layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(set = 0, binding = 2, r32f) uniform writeonly image2D colsMatrix;
layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 1, r32f) uniform writeonly image2D rowsMatrix;

shared float sharedfloat[1024];

void main() {
  sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] =
      imageLoad(_input, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x),
                              (32 * int(gl_WorkGroupID.y)) + int(gl_LocalInvocationID.y)))
          .x;
  sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] =
      imageLoad(_input, ivec2(((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x)) + 16,
                              (32 * int(gl_WorkGroupID.y)) + int(gl_LocalInvocationID.y)))
          .x;
  barrier();
  if (gl_LocalInvocationID.x == 0u) {
    float _94;
    _94 = 0.0;
    for (uint _95 = 0u; _95 < 32u;) {
      _94 += sharedfloat[_95 + (32u * gl_LocalInvocationID.y)];
      _95++;
      continue;
    }
    imageStore(rowsMatrix, ivec2(uvec2(gl_WorkGroupID.x, (32u * gl_WorkGroupID.y) + gl_LocalInvocationID.y)), vec4(_94));
  } else {
  }
  barrier();
  if (gl_LocalInvocationID.x < gl_LocalInvocationID.y) {
    float _119 = sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)];
    sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] =
        sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)];
    sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)] = _119;
  } else {
  }
  if ((gl_LocalInvocationID.x + 16u) < gl_LocalInvocationID.y) {
    float _150 = sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)];
    sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] =
        sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))];
    sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))] = _150;
  } else {
  }
  barrier();
  if (gl_LocalInvocationID.x == 0u) {
    float _186;
    _186 = 0.0;
    for (uint _187 = 0u; _187 < 32u;) {
      _186 += sharedfloat[_187 + (32u * gl_LocalInvocationID.y)];
      _187++;
      continue;
    }
    imageStore(colsMatrix, ivec2(uvec2((32u * gl_WorkGroupID.x) + gl_LocalInvocationID.y, gl_WorkGroupID.y)), vec4(_186));
  } else {
  }
}
