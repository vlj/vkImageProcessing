#version 450

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#extension GL_ARB_tessellation_shader : enable

#include "common.h"


void main()
{
    loadToSharedMem();

    partialHorizontalPrefixSumInSharedMemory();
    partialVerticalPrefixSumInSharedMemory();

    ivec3 localId = ivec3(gl_LocalInvocationID);
    int x = localId.x;

    LocalAndGlobal ids = GetLocalAndGlobal();
    int gid_x = 32 * int(gl_WorkGroupID.x);
    int gid_y = 32 * (line - int(gl_WorkGroupID.x));

	float corner = imageLoad(cs_output, ivec2(gid_x - 1, gid_y - 1)).x;


	{
		float border = imageLoad(cs_output, ivec2(gid_x + localId.x, gid_y - 1)).x;
		shared_floatx512[index2dto1d(localId.x, localId.y)] += border;
	}
	{
		float border = imageLoad(cs_output, ivec2(gid_x + localId.x + 16, gid_y - 1)).x;
		shared_floatx512[index2dto1d(localId.x + 16, localId.y)] += border;
	}

	{
		float border = imageLoad(cs_output, ivec2(gid_x - 1, gid_y + localId.y)).x;
		shared_floatx512[index2dto1d(localId.x, localId.y)] += border;
		shared_floatx512[index2dto1d(localId.x + 16, localId.y)] += border;
	}
	{
		shared_floatx512[index2dto1d(localId.x, localId.y)] -= corner;
		shared_floatx512[index2dto1d(localId.x + 16, localId.y)] -= corner;
	}

    writeFromSharedMem();
}