#include "ShadersCollection.h"
    
    const std::vector<uint32_t> v2::guidedFilterPass2_h_spv::bytecode = {
        0x7230203,0x10300,0xd000a,0xd3,0x0,0x20011,0x1,0x20011,0x3d,0x20011,0x3f,0x6000b,0x1,0x4c534c47,0x6474732e,0x3035342e,0x0,0x3000e,0x0,0x1,0x7000f,0x5,0x4,0x6e69616d,0x0,0x23,0x28,0x60010,0x4,0x11,0x8,0x10,0x1,0x30003,0x2,0x1c2,0xa0004,0x475f4c47,0x4c474f4f,0x70635f45,0x74735f70,0x5f656c79,0x656e696c,0x7269645f,0x69746365,0x6576,0x80004,0x475f4c47,0x4c474f4f,0x6e695f45,0x64756c63,0x69645f65,0x74636572,0x657669,0xa0004,0x4b5f4c47,0x735f5248,0x65646168,0x75735f72,0x6f726762,0x615f7075,0x68746972,0x6974656d,0x63,0x90004,0x4b5f4c47,0x735f5248,0x65646168,0x75735f72,0x6f726762,0x625f7075,0x63697361,0x0,0x40005,0x4,0x6e69616d,0x0,0x30005,0xc,0x35315f,0x30005,0xe,0x30315f,0x30005,0x11,0x34375f,0x30005,0x1c,0x39335f,0x40005,0x1f,0x706e695f,0x7475,0x60005,0x23,0x575f6c67,0x476b726f,0x70756f72,0x4449,0x80005,0x28,0x535f6c67,0x72676275,0x4970756f,0x636f766e,0x6f697461,0x44496e,0x30005,0x3a,0x31345f,0x90005,0x44,0x6d6d7573,0x6f436465,0x6e6d756c,0x75646552,0x4d646563,0x69727461,0x78,0xb0005,0x54,0x6d6d7573,0x71536465,0x65726175,0x6c6f4364,0x526e6d75,0x63756465,0x614d6465,0x78697274,0x0,0x30005,0x69,0x33375f,0x40005,0x6d,0x3334315f,0x0,0x40005,0x70,0x3434315f,0x0,0x30005,0x78,0x35385f,0x80005,0x80,0x6d6d7573,0x6f526465,0x64655277,0x64656375,0x7274614d,0x7869,0xa0005,0x8f,0x6d6d7573,0x71536465,0x65726175,0x776f5264,0x75646552,0x4d646563,0x69727461,0x78,0x50005,0xa1,0x7074756f,0x654d7475,0x6e61,0x70005,0xb7,0x7074756f,0x71537475,0x65726175,0x61654d64,0x6e,0x40005,0xcc,0x3234315f,0x0,0x40047,0x1f,0x22,0x0,0x40047,0x1f,0x21,0x0,0x30047,0x1f,0x18,0x40047,0x23,0xb,0x1a,0x30047,0x28,0x0,0x40047,0x28,0xb,0x29,0x30047,0x29,0x0,0x40047,0x44,0x22,0x0,0x40047,0x44,0x21,0x1,0x30047,0x44,0x18,0x40047,0x54,0x22,0x0,0x40047,0x54,0x21,0x2,0x30047,0x54,0x18,0x40047,0x80,0x22,0x0,0x40047,0x80,0x21,0x3,0x30047,0x80,0x18,0x30047,0x85,0x0,0x40047,0x8f,0x22,0x0,0x40047,0x8f,0x21,0x4,0x30047,0x8f,0x18,0x30047,0x94,0x0,0x40047,0xa1,0x22,0x0,0x40047,0xa1,0x21,0x5,0x30047,0xa1,0x19,0x30047,0xa8,0x0,0x40047,0xb7,0x22,0x0,0x40047,0xb7,0x21,0x6,0x30047,0xb7,0x19,0x30047,0xbd,0x0,0x40047,0xd2,0xb,0x19,0x20013,0x2,0x30021,0x3,0x2,0x30016,0x6,0x20,0x40017,0x7,0x6,0x2,0x40015,0x8,0x20,0x0,0x4002b,0x8,0x9,0x10,0x4001c,0xa,0x7,0x9,0x40020,0xb,0x7,0xa,0x40020,0xd,0x6,0xa,0x4003b,0xd,0xe,0x6,0x40020,0x10,0x7,0x8,0x4002b,0x8,0x12,0x0,0x20014,0x19,0x40020,0x1b,0x7,0x6,0x90019,0x1d,0x6,0x1,0x0,0x0,0x0,0x2,0x3,0x40020,0x1e,0x0,0x1d,0x4003b,0x1e,0x1f,0x0,0x40017,0x21,0x8,0x3,0x40020,0x22,0x1,0x21,0x4003b,0x22,0x23,0x1,0x40020,0x24,0x1,0x8,0x4003b,0x24,0x28,0x1,0x40015,0x2b,0x20,0x1,0x4002b,0x8,0x2d,0x1,0x40017,0x34,0x2b,0x2,0x40017,0x36,0x6,0x4,0x40020,0x39,0x7,0x7,0x4003b,0x1e,0x44,0x0,0x4002b,0x2b,0x49,0x1,0x4003b,0x1e,0x54,0x0,0x4002b,0x8,0x65,0x3,0x4002b,0x6,0x6e,0x0,0x5002c,0x7,0x6f,0x6e,0x6e,0x4003b,0x1e,0x80,0x0,0x4003b,0x1e,0x8f,0x0,0x4003b,0x1e,0xa1,0x0,0x4002b,0x2b,0xa3,0x10,0x4003b,0x1e,0xb7,0x0,0x4002b,0x8,0xd1,0x8,0x6002c,0x21,0xd2,0xd1,0x9,0x2d,0x50036,0x2,0x4,0x0,0x3,0x200f8,0x5,0x4003b,0xb,0xc,0x7,0x4003b,0x10,0x11,0x7,0x4003b,0x1b,0x1c,0x7,0x4003b,0x39,0x3a,0x7,0x4003b,0x10,0x69,0x7,0x4003b,0x39,0x6d,0x7,0x4003b,0x10,0x70,0x7,0x4003b,0x39,0x78,0x7,0x4003b,0x10,0xcc,0x7,0x4003d,0xa,0xf,0xe,0x3003e,0xc,0xf,0x3003e,0x11,0x12,0x200f9,0x13,0x200f8,0x13,0x400f6,0x15,0x16,0x0,0x200f9,0x17,0x200f8,0x17,0x4003d,0x8,0x18,0x11,0x500b0,0x19,0x1a,0x18,0x9,0x400fa,0x1a,0x14,0x15,0x200f8,0x14,0x4003d,0x1d,0x20,0x1f,0x50041,0x24,0x25,0x23,0x12,0x4003d,0x8,0x26,0x25,0x50084,0x8,0x27,0x9,0x26,0x4003d,0x8,0x29,0x28,0x50080,0x8,0x2a,0x27,0x29,0x4007c,0x2b,0x2c,0x2a,0x50041,0x24,0x2e,0x23,0x2d,0x4003d,0x8,0x2f,0x2e,0x50084,0x8,0x30,0x9,0x2f,0x4003d,0x8,0x31,0x11,0x50080,0x8,0x32,0x30,0x31,0x4007c,0x2b,0x33,0x32,0x50050,0x34,0x35,0x2c,0x33,0x50062,0x36,0x37,0x20,0x35,0x50051,0x6,0x38,0x37,0x0,0x3003e,0x1c,0x38,0x4003d,0x6,0x3b,0x1c,0x4003d,0x6,0x3c,0x1c,0x4003d,0x6,0x3d,0x1c,0x50085,0x6,0x3e,0x3c,0x3d,0x50050,0x7,0x3f,0x3b,0x3e,0x3003e,0x3a,0x3f,0x4003d,0x8,0x40,0x11,0x4003d,0x7,0x41,0x3a,0x50041,0x39,0x42,0xc,0x40,0x3003e,0x42,0x41,0x4003d,0x8,0x43,0x11,0x4003d,0x1d,0x45,0x44,0x50041,0x24,0x46,0x23,0x12,0x4003d,0x8,0x47,0x46,0x4007c,0x2b,0x48,0x47,0x50082,0x2b,0x4a,0x48,0x49,0x50041,0x24,0x4b,0x23,0x2d,0x4003d,0x8,0x4c,0x4b,0x50084,0x8,0x4d,0x9,0x4c,0x4003d,0x8,0x4e,0x11,0x50080,0x8,0x4f,0x4d,0x4e,0x4007c,0x2b,0x50,0x4f,0x50050,0x34,0x51,0x4a,0x50,0x50062,0x36,0x52,0x45,0x51,0x50051,0x6,0x53,0x52,0x0,0x4003d,0x1d,0x55,0x54,0x50041,0x24,0x56,0x23,0x12,0x4003d,0x8,0x57,0x56,0x4007c,0x2b,0x58,0x57,0x50082,0x2b,0x59,0x58,0x49,0x50041,0x24,0x5a,0x23,0x2d,0x4003d,0x8,0x5b,0x5a,0x50084,0x8,0x5c,0x9,0x5b,0x4003d,0x8,0x5d,0x11,0x50080,0x8,0x5e,0x5c,0x5d,0x4007c,0x2b,0x5f,0x5e,0x50050,0x34,0x60,0x59,0x5f,0x50062,0x36,0x61,0x55,0x60,0x50051,0x6,0x62,0x61,0x0,0x50050,0x7,0x63,0x53,0x62,0x4003d,0x7,0x64,0x3a,0x6015e,0x7,0x66,0x65,0x1,0x64,0x50081,0x7,0x67,0x63,0x66,0x50041,0x39,0x68,0xc,0x43,0x3003e,0x68,0x67,0x4003d,0x8,0x6a,0x11,0x50080,0x8,0x6b,0x6a,0x2d,0x3003e,0x69,0x6b,0x200f9,0x16,0x200f8,0x16,0x4003d,0x8,0x6c,0x69,0x3003e,0x11,0x6c,0x200f9,0x13,0x200f8,0x15,0x3003e,0x6d,0x6f,0x3003e,0x70,0x12,0x200f9,0x71,0x200f8,0x71,0x400f6,0x73,0x74,0x0,0x200f9,0x75,0x200f8,0x75,0x4003d,0x8,0x76,0x70,0x500b0,0x19,0x77,0x76,0x9,0x400fa,0x77,0x72,0x73,0x200f8,0x72,0x4003d,0x8,0x79,0x70,0x50041,0x39,0x7a,0xc,0x79,0x4003d,0x7,0x7b,0x7a,0x4003d,0x7,0x7c,0x6d,0x50081,0x7,0x7d,0x7b,0x7c,0x3003e,0x78,0x7d,0x4003d,0x8,0x7e,0x70,0x4003d,0x7,0x7f,0x78,0x4003d,0x1d,0x81,0x80,0x50041,0x24,0x82,0x23,0x12,0x4003d,0x8,0x83,0x82,0x50084,0x8,0x84,0x83,0x9,0x4003d,0x8,0x85,0x28,0x50080,0x8,0x86,0x84,0x85,0x4007c,0x2b,0x87,0x86,0x50041,0x24,0x88,0x23,0x2d,0x4003d,0x8,0x89,0x88,0x4007c,0x2b,0x8a,0x89,0x50082,0x2b,0x8b,0x8a,0x49,0x50050,0x34,0x8c,0x87,0x8b,0x50062,0x36,0x8d,0x81,0x8c,0x50051,0x6,0x8e,0x8d,0x0,0x4003d,0x1d,0x90,0x8f,0x50041,0x24,0x91,0x23,0x12,0x4003d,0x8,0x92,0x91,0x50084,0x8,0x93,0x92,0x9,0x4003d,0x8,0x94,0x28,0x50080,0x8,0x95,0x93,0x94,0x4007c,0x2b,0x96,0x95,0x50041,0x24,0x97,0x23,0x2d,0x4003d,0x8,0x98,0x97,0x4007c,0x2b,0x99,0x98,0x50082,0x2b,0x9a,0x99,0x49,0x50050,0x34,0x9b,0x96,0x9a,0x50062,0x36,0x9c,0x90,0x9b,0x50051,0x6,0x9d,0x9c,0x0,0x50050,0x7,0x9e,0x8e,0x9d,0x50081,0x7,0x9f,0x7f,0x9e,0x50041,0x39,0xa0,0xc,0x7e,0x3003e,0xa0,0x9f,0x4003d,0x1d,0xa2,0xa1,0x50041,0x24,0xa4,0x23,0x12,0x4003d,0x8,0xa5,0xa4,0x4007c,0x2b,0xa6,0xa5,0x50084,0x2b,0xa7,0xa3,0xa6,0x4003d,0x8,0xa8,0x28,0x4007c,0x2b,0xa9,0xa8,0x50080,0x2b,0xaa,0xa7,0xa9,0x50041,0x24,0xab,0x23,0x2d,0x4003d,0x8,0xac,0xab,0x4007c,0x2b,0xad,0xac,0x50084,0x2b,0xae,0xa3,0xad,0x4003d,0x8,0xaf,0x70,0x4007c,0x2b,0xb0,0xaf,0x50080,0x2b,0xb1,0xae,0xb0,0x50050,0x34,0xb2,0xaa,0xb1,0x4003d,0x8,0xb3,0x70,0x60041,0x1b,0xb4,0xc,0xb3,0x12,0x4003d,0x6,0xb5,0xb4,0x70050,0x36,0xb6,0xb5,0xb5,0xb5,0xb5,0x40063,0xa2,0xb2,0xb6,0x4003d,0x1d,0xb8,0xb7,0x50041,0x24,0xb9,0x23,0x12,0x4003d,0x8,0xba,0xb9,0x4007c,0x2b,0xbb,0xba,0x50084,0x2b,0xbc,0xa3,0xbb,0x4003d,0x8,0xbd,0x28,0x4007c,0x2b,0xbe,0xbd,0x50080,0x2b,0xbf,0xbc,0xbe,0x50041,0x24,0xc0,0x23,0x2d,0x4003d,0x8,0xc1,0xc0,0x4007c,0x2b,0xc2,0xc1,0x50084,0x2b,0xc3,0xa3,0xc2,0x4003d,0x8,0xc4,0x70,0x4007c,0x2b,0xc5,0xc4,0x50080,0x2b,0xc6,0xc3,0xc5,0x50050,0x34,0xc7,0xbf,0xc6,0x4003d,0x8,0xc8,0x70,0x60041,0x1b,0xc9,0xc,0xc8,0x2d,0x4003d,0x6,0xca,0xc9,0x70050,0x36,0xcb,0xca,0xca,0xca,0xca,0x40063,0xb8,0xc7,0xcb,0x4003d,0x8,0xcd,0x70,0x50080,0x8,0xce,0xcd,0x2d,0x3003e,0xcc,0xce,0x200f9,0x74,0x200f8,0x74,0x4003d,0x7,0xcf,0x78,0x3003e,0x6d,0xcf,0x4003d,0x8,0xd0,0xcc,0x3003e,0x70,0xd0,0x200f9,0x71,0x200f8,0x73,0x100fd,0x10038
    };