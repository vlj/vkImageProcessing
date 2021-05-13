#version 450
layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

layout(set = 0, binding = 2, r32f) uniform readonly image2D colsMatrix;
layout(set = 0, binding = 0, r32f) uniform readonly image2D _input;
layout(set = 0, binding = 3, r32f) uniform writeonly image2D _output;
layout(set = 0, binding = 1, r32f) uniform readonly image2D rowsMatrix;

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
  int _118;
  int _119;
  uint _120;
  _118 = 1;
  _119 = int((2u * gl_LocalInvocationID.x) + 1u);
  _120 = 0u;
  for (;;) {
    if (_120 < 5u) {
      if (((_119 + _118) - 1) < 32) {
        sharedfloat[((_119 + _118) - 1) + (32 * int(gl_LocalInvocationID.y))] =
            sharedfloat[(_119 - 1) + (32 * int(gl_LocalInvocationID.y))] +
            sharedfloat[((_119 + _118) - 1) + (32 * int(gl_LocalInvocationID.y))];
      } else {
      }
      barrier();
      _118 = 2 * _118;
      _119 = 2 * _119;
      _120 = 1u + _120;
      continue;
    } else {
      break;
    }
  }
  if (gl_LocalInvocationID.x == 0u) {
    sharedfloat[31u + (32u * gl_LocalInvocationID.y)] = sharedfloat[0u + (32u * gl_LocalInvocationID.y)] + 0.0;
  } else {
  }
  int _188;
  int _189;
  uint _190;
  _188 = 16;
  _189 = int((32u * gl_LocalInvocationID.x) + 16u);
  _190 = 0u;
  for (;;) {
    if (_190 < 6u) {
      if (((_189 + _188) - 1) < 32) {
        int _161 = ((_189 + _188) - 1) + (32 * int(gl_LocalInvocationID.y));
        float _163 = sharedfloat[_161];
        sharedfloat[((_189 + _188) - 1) + (32 * int(gl_LocalInvocationID.y))] =
            _163 + sharedfloat[(_189 - 1) + (32 * int(gl_LocalInvocationID.y))];
        sharedfloat[(_189 - 1) + (32 * int(gl_LocalInvocationID.y))] = _163;
      } else {
      }
      barrier();
      _188 /= 2;
      _189 /= 2;
      _190 = 1u + _190;
      continue;
    } else {
      break;
    }
  }
  sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] +=
      imageLoad(rowsMatrix, ivec2(int(gl_WorkGroupID.x) - 1, (32 * int(gl_WorkGroupID.y)) + int(gl_LocalInvocationID.y))).x;
  sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] +=
      imageLoad(rowsMatrix, ivec2(int(gl_WorkGroupID.x) - 1, (32 * int(gl_WorkGroupID.y)) + int(gl_LocalInvocationID.y))).x;
  barrier();
  if (gl_LocalInvocationID.x < gl_LocalInvocationID.y) {
    float _258 = sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)];
    sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] =
        sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)];
    sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)] = _258;
  } else {
  }
  if ((gl_LocalInvocationID.x + 16u) < gl_LocalInvocationID.y) {
    float _289 = sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)];
    sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] =
        sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))];
    sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))] = _289;
  } else {
  }
  barrier();
  int _349;
  int _350;
  uint _351;
  _349 = 1;
  _350 = int((2u * gl_LocalInvocationID.x) + 1u);
  _351 = 0u;
  for (;;) {
    if (_351 < 5u) {
      if (((_350 + _349) - 1) < 32) {
        sharedfloat[((_350 + _349) - 1) + (32 * int(gl_LocalInvocationID.y))] =
            sharedfloat[(_350 - 1) + (32 * int(gl_LocalInvocationID.y))] +
            sharedfloat[((_350 + _349) - 1) + (32 * int(gl_LocalInvocationID.y))];
      } else {
      }
      barrier();
      _349 = 2 * _349;
      _350 = 2 * _350;
      _351 = 1u + _351;
      continue;
    } else {
      break;
    }
  }
  if (gl_LocalInvocationID.x == 0u) {
    sharedfloat[31u + (32u * gl_LocalInvocationID.y)] = sharedfloat[0u + (32u * gl_LocalInvocationID.y)] + 0.0;
  } else {
  }
  int _416;
  int _417;
  uint _418;
  _416 = 16;
  _417 = int((32u * gl_LocalInvocationID.x) + 16u);
  _418 = 0u;
  for (;;) {
    if (_418 < 6u) {
      if (((_417 + _416) - 1) < 32) {
        int _389 = ((_417 + _416) - 1) + (32 * int(gl_LocalInvocationID.y));
        float _391 = sharedfloat[_389];
        sharedfloat[((_417 + _416) - 1) + (32 * int(gl_LocalInvocationID.y))] =
            _391 + sharedfloat[(_417 - 1) + (32 * int(gl_LocalInvocationID.y))];
        sharedfloat[(_417 - 1) + (32 * int(gl_LocalInvocationID.y))] = _391;
      } else {
      }
      barrier();
      _416 /= 2;
      _417 /= 2;
      _418 = 1u + _418;
      continue;
    } else {
      break;
    }
  }
  if (gl_LocalInvocationID.x < gl_LocalInvocationID.y) {
    float _432 = sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)];
    sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] =
        sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)];
    sharedfloat[gl_LocalInvocationID.y + (32u * gl_LocalInvocationID.x)] = _432;
  } else {
  }
  if ((gl_LocalInvocationID.x + 16u) < gl_LocalInvocationID.y) {
    float _463 = sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)];
    sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] =
        sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))];
    sharedfloat[gl_LocalInvocationID.y + (32u * (gl_LocalInvocationID.x + 16u))] = _463;
  } else {
  }
  barrier();
  sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)] +=
      imageLoad(colsMatrix, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x), int(gl_WorkGroupID.y) - 1)).x;
  sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)] +=
      imageLoad(colsMatrix, ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x + 16u), int(gl_WorkGroupID.y) - 1)).x;
  imageStore(_output,
             ivec2((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x), (32 * int(gl_WorkGroupID.y)) + int(gl_LocalInvocationID.y)),
             vec4(sharedfloat[gl_LocalInvocationID.x + (32u * gl_LocalInvocationID.y)]));
  imageStore(
      _output,
      ivec2(((32 * int(gl_WorkGroupID.x)) + int(gl_LocalInvocationID.x)) + 16, (32 * int(gl_WorkGroupID.y)) + int(gl_LocalInvocationID.y)),
      vec4(sharedfloat[(gl_LocalInvocationID.x + 16u) + (32u * gl_LocalInvocationID.y)]));
}
