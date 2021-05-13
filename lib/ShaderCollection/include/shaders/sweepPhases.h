

void UpSweepPhase(int x)
{
    int curr_x = 2 * x + 1;
    int curr_offset = 1;
    for (int i = 0; (i < 5); i++)
    {
        barrier();
        int write_idx = ((curr_x + curr_offset) - 1);
        int read_idx = (curr_x - 1);
        if ((write_idx < 32))
        {
            shared_floatx512[index2dto1d(write_idx, int(gl_LocalInvocationID.y))] = (shared_floatx512[index2dto1d(read_idx, int(gl_LocalInvocationID.y))] + shared_floatx512[index2dto1d(write_idx, int(gl_LocalInvocationID.y))]);
        }
        curr_x = 2 * curr_x;
        curr_offset = 2 * curr_offset;
    }
}

void DownSweepPhase(int x)
{
    int curr_x = 32 * x + 16;
    int curr_offset = 16;
    for (int i1 = 0; (i1 < 6); i1++)
    {
        barrier();
        int write_idx1 = ((curr_x + curr_offset) - 1);
        int move_dest = (curr_x - 1);
        if ((write_idx1 < 32))
        {
            float temp = shared_floatx512[index2dto1d(write_idx1, int(gl_LocalInvocationID.y))];
            shared_floatx512[index2dto1d(write_idx1, int(gl_LocalInvocationID.y))] = (shared_floatx512[index2dto1d(move_dest, int(gl_LocalInvocationID.y))] + shared_floatx512[index2dto1d(write_idx1, int(gl_LocalInvocationID.y))]);
            shared_floatx512[index2dto1d(move_dest, int(gl_LocalInvocationID.y))] = temp;
        }
        curr_x = curr_x / 2;
        curr_offset = curr_offset / 2;
    }
}