

void partialHorizontalPrefixSumInSharedMemory()
{
    ivec3 localId = ivec3(gl_LocalInvocationID);
    int x = localId.x;

    UpSweepPhase(x);

    if ((x == 0))
    {
        shared_floatx512[index2dto1d(31, int(gl_LocalInvocationID.y))] = shared_floatx512[index2dto1d(0, localId.y)];
    }
    DownSweepPhase(x);

}

void partialVerticalPrefixSumInSharedMemory()
{
    ivec3 localId = ivec3(gl_LocalInvocationID);
    int x = localId.x;

    transposeSharedMemory();

    UpSweepPhase(x);

    if ((x == 0))
    {
        shared_floatx512[index2dto1d(31, localId.y)] = shared_floatx512[index2dto1d(0, localId.y)];
    }
    DownSweepPhase(x);

    transposeSharedMemory();
}


void horizontalPrefixSumInSharedMemory()
{
    ivec3 localId = ivec3(gl_LocalInvocationID);
    int x = localId.x;

    UpSweepPhase(x);

    LocalAndGlobal ids = GetLocalAndGlobal();
    int gid_x = 32 * int(gl_WorkGroupID.x);
    int gid_y = 32 * (line - int(gl_WorkGroupID.x));

    if ((x == 0))
    {
        shared_floatx512[index2dto1d(31, localId.y)] = imageLoad(cs_output, ivec2(gid_x - 1, ids.globalId.y)).x + shared_floatx512[index2dto1d(0, localId.y)];
    }
    DownSweepPhase(x);

}

void verticalPrefixSumInSharedMemory()
{
    transposeSharedMemory();

    ivec3 localId = ivec3(gl_LocalInvocationID);
    int x = localId.x;

    UpSweepPhase(x);

    LocalAndGlobal ids = GetLocalAndGlobal();
    int gid_x = 32 * int(gl_WorkGroupID.x);
    int gid_y = 32 * (line - int(gl_WorkGroupID.x));

    if ((x == 0))
    {
        // here x and y are inverted !
        shared_floatx512[index2dto1d(31, localId.y)] = imageLoad(cs_output, ivec2(gid_x + localId.y, gid_y - 1)).x + shared_floatx512[index2dto1d(0, localId.y)];
    }
    DownSweepPhase(x);

    transposeSharedMemory();
}