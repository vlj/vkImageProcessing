#version 450

#define SOURCECOUNT 1
layout(binding= 0) uniform sampler2D someSampler;

layout(binding=1, rgba16f) uniform image2D result;

layout(push_constant) uniform UBO {
    vec2 someInput[2];
} ubo;

vec2 dstCoord() {
    return gl_GlobalInvocationID.xy;
}

void main()
{
	ivec2 idx = ivec2(gl_GlobalInvocationID.xy);
	vec4 inputTexel = texture(someSampler, idx + ubo.someInput[0]);
    imageStore(result, idx, inputTexel);
}
