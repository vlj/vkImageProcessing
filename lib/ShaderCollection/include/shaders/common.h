
// Shared memory

shared float shared_floatx512[33 * 32];

int index2dto1d(int x, int y)
{
    return (x * 33 + (y));
}

// Inputs

layout(set = 0, binding = 0, r32f)
uniform image2D cs_input;

layout(set = 0, binding = 1, r32f)
uniform image2D cs_output;

layout(set = 1, binding = 0, std140) uniform MainBlock
{
    int line;
};

// Computation geometry

layout(local_size_x = 16, local_size_y = 32, local_size_z = 1) in;

struct LocalAndGlobal
{
    ivec2 localId;
    ivec2 globalId;
};

LocalAndGlobal GetLocalAndGlobal()
{
    LocalAndGlobal result;
    result.localId = ivec3(gl_LocalInvocationID).xy;
    int gid_x = 32 * int(gl_WorkGroupID.x) + result.localId.x;
    int gid_y = 32 * (line - int(gl_WorkGroupID.x)) + result.localId.y;
    result.globalId = ivec2(gid_x, gid_y);
    return result;
}

// Shared memory upload/download

void loadToSharedMem()
{
    LocalAndGlobal ids = GetLocalAndGlobal();
    ivec2 globalId = ids.globalId;
    ivec2 localId = ids.localId;
    int x = localId.x;
    ivec2 pixel1_coord = globalId;
    ivec2 pixel2_coord = ivec2(globalId.x + 16, globalId.y);
    shared_floatx512[index2dto1d(x, localId.y)] = imageLoad(cs_input, pixel1_coord).x;
    shared_floatx512[index2dto1d(x + 16, localId.y)] = imageLoad(cs_input, pixel2_coord).x;
}

void writeFromSharedMem()
{
    LocalAndGlobal ids = GetLocalAndGlobal();
    ivec2 globalId = ids.globalId;
    ivec2 localId = ids.localId;
    int x = localId.x;
    ivec2 pixel1_coord = globalId;
    ivec2 pixel2_coord = ivec2(globalId.x + 16, globalId.y);
    imageStore(cs_output, pixel1_coord, vec4(shared_floatx512[index2dto1d(x, localId.y)]));
    imageStore(cs_output, pixel2_coord, vec4(shared_floatx512[index2dto1d(x + 16, localId.y)]));
}

// Algo

void swapSharedMemory(int x, int y)
{

    float tmp = shared_floatx512[index2dto1d(x, y)];
    shared_floatx512[index2dto1d(x, y)] = shared_floatx512[index2dto1d(y, x)];
    shared_floatx512[index2dto1d(y, x)] = tmp;
}

void transposeSharedMemory()
{
    ivec3 localId = ivec3(gl_LocalInvocationID);
    int y = localId.y;
    int x = localId.x;

    barrier();
    if (x < y)
    {
        swapSharedMemory(x, y);
    }
    barrier();
    if (x + 16 < y)
    {
        swapSharedMemory(x + 16, y);
    }
    barrier();
}


#include "sweepPhases.h"
#include "prefixSum.h"