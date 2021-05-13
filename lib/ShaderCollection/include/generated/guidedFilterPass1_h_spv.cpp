#include "ShadersCollection.h"
    
    const std::vector<uint32_t> Shaders::guidedFilterPass1_h_spv::bytecode = {
        0x7230203,0x10300,0xd000a,0x9f,0x0,0x20011,0x1,0x20011,0x3d,0x20011,0x3f,0x6000b,0x1,0x4c534c47,0x6474732e,0x3035342e,0x0,0x3000e,0x0,0x1,0x7000f,0x5,0x4,0x6e69616d,0x0,0x23,0x30,0x60010,0x4,0x11,0x1,0x10,0x1,0x30003,0x2,0x1c2,0xa0004,0x475f4c47,0x4c474f4f,0x70635f45,0x74735f70,0x5f656c79,0x656e696c,0x7269645f,0x69746365,0x6576,0x80004,0x475f4c47,0x4c474f4f,0x6e695f45,0x64756c63,0x69645f65,0x74636572,0x657669,0xa0004,0x4b5f4c47,0x735f5248,0x65646168,0x75735f72,0x6f726762,0x615f7075,0x68746972,0x6974656d,0x63,0x90004,0x4b5f4c47,0x735f5248,0x65646168,0x75735f72,0x6f726762,0x625f7075,0x63697361,0x0,0x40005,0x4,0x6e69616d,0x0,0x30005,0xc,0x35315f,0x30005,0xe,0x30315f,0x30005,0x11,0x31375f,0x30005,0x1c,0x39335f,0x40005,0x1f,0x706e695f,0x7475,0x60005,0x23,0x575f6c67,0x476b726f,0x70756f72,0x4449,0x80005,0x30,0x535f6c67,0x72676275,0x4970756f,0x636f766e,0x6f697461,0x44496e,0x30005,0x3a,0x31345f,0x30005,0x43,0x35345f,0x70005,0x4b,0x756c6f63,0x65526e6d,0x65637564,0x74614d64,0x786972,0x90005,0x5a,0x61757173,0x43646572,0x6d756c6f,0x6465526e,0x64656375,0x7274614d,0x7869,0x30005,0x68,0x30375f,0x30005,0x6c,0x33385f,0x30005,0x6f,0x34385f,0x30005,0x77,0x31385f,0x30005,0x7d,0x32385f,0x70005,0x82,0x52776f72,0x63756465,0x614d6465,0x78697274,0x0,0x80005,0x90,0x61757173,0x52646572,0x6552776f,0x65637564,0x74614d64,0x786972,0x40047,0x1f,0x22,0x0,0x40047,0x1f,0x21,0x0,0x30047,0x1f,0x18,0x40047,0x23,0xb,0x1a,0x30047,0x30,0x0,0x40047,0x30,0xb,0x29,0x30047,0x31,0x0,0x30047,0x47,0x0,0x40047,0x4b,0x22,0x0,0x40047,0x4b,0x21,0x1,0x30047,0x4b,0x19,0x40047,0x5a,0x22,0x0,0x40047,0x5a,0x21,0x2,0x30047,0x5a,0x19,0x40047,0x82,0x22,0x0,0x40047,0x82,0x21,0x3,0x30047,0x82,0x19,0x30047,0x87,0x0,0x40047,0x90,0x22,0x0,0x40047,0x90,0x21,0x4,0x30047,0x90,0x19,0x30047,0x95,0x0,0x40047,0x9e,0xb,0x19,0x20013,0x2,0x30021,0x3,0x2,0x30016,0x6,0x20,0x40017,0x7,0x6,0x2,0x40015,0x8,0x20,0x0,0x4002b,0x8,0x9,0x10,0x4001c,0xa,0x7,0x9,0x40020,0xb,0x7,0xa,0x40020,0xd,0x6,0xa,0x4003b,0xd,0xe,0x6,0x40020,0x10,0x7,0x8,0x4002b,0x8,0x12,0x0,0x20014,0x19,0x40020,0x1b,0x7,0x6,0x90019,0x1d,0x6,0x1,0x0,0x0,0x0,0x2,0x3,0x40020,0x1e,0x0,0x1d,0x4003b,0x1e,0x1f,0x0,0x40017,0x21,0x8,0x3,0x40020,0x22,0x1,0x21,0x4003b,0x22,0x23,0x1,0x40020,0x24,0x1,0x8,0x40015,0x2a,0x20,0x1,0x4002b,0x8,0x2c,0x1,0x4003b,0x24,0x30,0x1,0x40017,0x34,0x2a,0x2,0x40017,0x36,0x6,0x4,0x40020,0x39,0x7,0x7,0x4002b,0x8,0x45,0x3,0x4003b,0x1e,0x4b,0x0,0x40017,0x54,0x8,0x2,0x4003b,0x1e,0x5a,0x0,0x4002b,0x6,0x6d,0x0,0x5002c,0x7,0x6e,0x6d,0x6d,0x4003b,0x1e,0x82,0x0,0x4003b,0x1e,0x90,0x0,0x6002c,0x21,0x9e,0x2c,0x9,0x2c,0x50036,0x2,0x4,0x0,0x3,0x200f8,0x5,0x4003b,0xb,0xc,0x7,0x4003b,0x10,0x11,0x7,0x4003b,0x1b,0x1c,0x7,0x4003b,0x39,0x3a,0x7,0x4003b,0x39,0x43,0x7,0x4003b,0x10,0x68,0x7,0x4003b,0x39,0x6c,0x7,0x4003b,0x10,0x6f,0x7,0x4003b,0x39,0x77,0x7,0x4003b,0x10,0x7d,0x7,0x4003d,0xa,0xf,0xe,0x3003e,0xc,0xf,0x3003e,0x11,0x12,0x200f9,0x13,0x200f8,0x13,0x400f6,0x15,0x16,0x0,0x200f9,0x17,0x200f8,0x17,0x4003d,0x8,0x18,0x11,0x500b0,0x19,0x1a,0x18,0x9,0x400fa,0x1a,0x14,0x15,0x200f8,0x14,0x4003d,0x1d,0x20,0x1f,0x50041,0x24,0x25,0x23,0x12,0x4003d,0x8,0x26,0x25,0x50084,0x8,0x27,0x9,0x26,0x4003d,0x8,0x28,0x11,0x50080,0x8,0x29,0x27,0x28,0x4007c,0x2a,0x2b,0x29,0x50041,0x24,0x2d,0x23,0x2c,0x4003d,0x8,0x2e,0x2d,0x50084,0x8,0x2f,0x9,0x2e,0x4003d,0x8,0x31,0x30,0x50080,0x8,0x32,0x2f,0x31,0x4007c,0x2a,0x33,0x32,0x50050,0x34,0x35,0x2b,0x33,0x50062,0x36,0x37,0x20,0x35,0x50051,0x6,0x38,0x37,0x0,0x3003e,0x1c,0x38,0x4003d,0x6,0x3b,0x1c,0x4003d,0x6,0x3c,0x1c,0x4003d,0x6,0x3d,0x1c,0x50085,0x6,0x3e,0x3c,0x3d,0x50050,0x7,0x3f,0x3b,0x3e,0x3003e,0x3a,0x3f,0x4003d,0x8,0x40,0x11,0x4003d,0x7,0x41,0x3a,0x50041,0x39,0x42,0xc,0x40,0x3003e,0x42,0x41,0x4003d,0x7,0x44,0x3a,0x6015e,0x7,0x46,0x45,0x0,0x44,0x3003e,0x43,0x46,0x4003d,0x8,0x47,0x30,0x500aa,0x19,0x48,0x47,0x12,0x300f7,0x4a,0x0,0x400fa,0x48,0x49,0x4a,0x200f8,0x49,0x4003d,0x1d,0x4c,0x4b,0x50041,0x24,0x4d,0x23,0x12,0x4003d,0x8,0x4e,0x4d,0x50041,0x24,0x4f,0x23,0x2c,0x4003d,0x8,0x50,0x4f,0x50084,0x8,0x51,0x9,0x50,0x4003d,0x8,0x52,0x11,0x50080,0x8,0x53,0x51,0x52,0x50050,0x54,0x55,0x4e,0x53,0x4007c,0x34,0x56,0x55,0x50041,0x1b,0x57,0x43,0x12,0x4003d,0x6,0x58,0x57,0x70050,0x36,0x59,0x58,0x58,0x58,0x58,0x40063,0x4c,0x56,0x59,0x4003d,0x1d,0x5b,0x5a,0x50041,0x24,0x5c,0x23,0x12,0x4003d,0x8,0x5d,0x5c,0x50041,0x24,0x5e,0x23,0x2c,0x4003d,0x8,0x5f,0x5e,0x50084,0x8,0x60,0x9,0x5f,0x4003d,0x8,0x61,0x11,0x50080,0x8,0x62,0x60,0x61,0x50050,0x54,0x63,0x5d,0x62,0x4007c,0x34,0x64,0x63,0x50041,0x1b,0x65,0x43,0x2c,0x4003d,0x6,0x66,0x65,0x70050,0x36,0x67,0x66,0x66,0x66,0x66,0x40063,0x5b,0x64,0x67,0x200f9,0x4a,0x200f8,0x4a,0x4003d,0x8,0x69,0x11,0x50080,0x8,0x6a,0x69,0x2c,0x3003e,0x68,0x6a,0x200f9,0x16,0x200f8,0x16,0x4003d,0x8,0x6b,0x68,0x3003e,0x11,0x6b,0x200f9,0x13,0x200f8,0x15,0x3003e,0x6c,0x6e,0x3003e,0x6f,0x12,0x200f9,0x70,0x200f8,0x70,0x400f6,0x72,0x73,0x0,0x200f9,0x74,0x200f8,0x74,0x4003d,0x8,0x75,0x6f,0x500b0,0x19,0x76,0x75,0x9,0x400fa,0x76,0x71,0x72,0x200f8,0x71,0x4003d,0x8,0x78,0x6f,0x50041,0x39,0x79,0xc,0x78,0x4003d,0x7,0x7a,0x79,0x4003d,0x7,0x7b,0x6c,0x50081,0x7,0x7c,0x7a,0x7b,0x3003e,0x77,0x7c,0x4003d,0x8,0x7e,0x6f,0x50080,0x8,0x7f,0x7e,0x2c,0x3003e,0x7d,0x7f,0x200f9,0x73,0x200f8,0x73,0x4003d,0x7,0x80,0x77,0x3003e,0x6c,0x80,0x4003d,0x8,0x81,0x7d,0x3003e,0x6f,0x81,0x200f9,0x70,0x200f8,0x72,0x4003d,0x1d,0x83,0x82,0x50041,0x24,0x84,0x23,0x12,0x4003d,0x8,0x85,0x84,0x50084,0x8,0x86,0x9,0x85,0x4003d,0x8,0x87,0x30,0x50080,0x8,0x88,0x86,0x87,0x50041,0x24,0x89,0x23,0x2c,0x4003d,0x8,0x8a,0x89,0x50050,0x54,0x8b,0x88,0x8a,0x4007c,0x34,0x8c,0x8b,0x50041,0x1b,0x8d,0x6c,0x12,0x4003d,0x6,0x8e,0x8d,0x70050,0x36,0x8f,0x8e,0x8e,0x8e,0x8e,0x40063,0x83,0x8c,0x8f,0x4003d,0x1d,0x91,0x90,0x50041,0x24,0x92,0x23,0x12,0x4003d,0x8,0x93,0x92,0x50084,0x8,0x94,0x9,0x93,0x4003d,0x8,0x95,0x30,0x50080,0x8,0x96,0x94,0x95,0x50041,0x24,0x97,0x23,0x2c,0x4003d,0x8,0x98,0x97,0x50050,0x54,0x99,0x96,0x98,0x4007c,0x34,0x9a,0x99,0x50041,0x1b,0x9b,0x6c,0x2c,0x4003d,0x6,0x9c,0x9b,0x70050,0x36,0x9d,0x9c,0x9c,0x9c,0x9c,0x40063,0x91,0x9a,0x9d,0x100fd,0x10038
    };