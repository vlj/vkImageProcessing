#version 450

#define SOURCECOUNT 1
layout(binding= 0) uniform sampler2D someSampler;

layout(binding=1, rgba8) uniform image2D result;

layout(push_constant) uniform UBO {
    vec2 someInput[2];
} ubo;

vec2 dstCoord() {
    return gl_GlobalInvocationID.xy;
}

void main()
{
	ivec2 idx = ivec2(gl_GlobalInvocationID.xy);
    vec4 inputTexel = textureLod(someSampler, (vec2(idx) + ubo.someInput[0]) / 1024 , 0.0) / 255;
    imageStore(result, idx, inputTexel);
}
