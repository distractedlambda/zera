pub const @"unreachable" = 0x00;
pub const nop = 0x01;
pub const block = 0x02;
pub const loop = 0x03;
pub const @"if" = 0x04;
pub const @"else" = 0x05;
pub const @"try" = 0x06;
pub const @"catch" = 0x07;
pub const throw = 0x08;
pub const rethrow = 0x09;
pub const end = 0x0b;
pub const br = 0x0c;
pub const br_if = 0x0d;
pub const br_table = 0x0e;
pub const @"return" = 0x0f;
pub const call = 0x10;
pub const call_indirect = 0x11;
pub const return_call = 0x12;
pub const return_call_indirect = 0x13;
pub const call_ref = 0x14;
pub const delegate = 0x18;
pub const catch_all = 0x19;
pub const drop = 0x1a;
pub const select = 0x1b;
pub const @"select t" = 0x1c;
pub const @"local.get" = 0x20;
pub const @"local.set" = 0x21;
pub const @"local.tee" = 0x22;
pub const @"global.get" = 0x23;
pub const @"global.set" = 0x24;
pub const @"table.get" = 0x25;
pub const @"table.set" = 0x26;
pub const @"i32.load" = 0x28;
pub const @"i64.load" = 0x29;
pub const @"f32.load" = 0x2a;
pub const @"f64.load" = 0x2b;
pub const @"i32.load8_s" = 0x2c;
pub const @"i32.load8_u" = 0x2d;
pub const @"i32.load16_s" = 0x2e;
pub const @"i32.load16_u" = 0x2f;
pub const @"i64.load8_s" = 0x30;
pub const @"i64.load8_u" = 0x31;
pub const @"i64.load16_s" = 0x32;
pub const @"i64.load16_u" = 0x33;
pub const @"i64.load32_s" = 0x34;
pub const @"i64.load32_u" = 0x35;
pub const @"i32.store" = 0x36;
pub const @"i64.store" = 0x37;
pub const @"f32.store" = 0x38;
pub const @"f64.store" = 0x39;
pub const @"i32.store8" = 0x3a;
pub const @"i32.store16" = 0x3b;
pub const @"i64.store8" = 0x3c;
pub const @"i64.store16" = 0x3d;
pub const @"i64.store32" = 0x3e;
pub const @"memory.size" = 0x3f;
pub const @"memory.grow" = 0x40;
pub const @"i32.const" = 0x41;
pub const @"i64.const" = 0x42;
pub const @"f32.const" = 0x43;
pub const @"f64.const" = 0x44;
pub const @"i32.eqz" = 0x45;
pub const @"i32.eq" = 0x46;
pub const @"i32.ne" = 0x47;
pub const @"i32.lt_s" = 0x48;
pub const @"i32.lt_u" = 0x49;
pub const @"i32.gt_s" = 0x4a;
pub const @"i32.gt_u" = 0x4b;
pub const @"i32.le_s" = 0x4c;
pub const @"i32.le_u" = 0x4d;
pub const @"i32.ge_s" = 0x4e;
pub const @"i32.ge_u" = 0x4f;
pub const @"i64.eqz" = 0x50;
pub const @"i64.eq" = 0x51;
pub const @"i64.ne" = 0x52;
pub const @"i64.lt_s" = 0x53;
pub const @"i64.lt_u" = 0x54;
pub const @"i64.gt_s" = 0x55;
pub const @"i64.gt_u" = 0x56;
pub const @"i64.le_s" = 0x57;
pub const @"i64.le_u" = 0x58;
pub const @"i64.ge_s" = 0x59;
pub const @"i64.ge_u" = 0x5a;
pub const @"f32.eq" = 0x5b;
pub const @"f32.ne" = 0x5c;
pub const @"f32.lt" = 0x5d;
pub const @"f32.gt" = 0x5e;
pub const @"f32.le" = 0x5f;
pub const @"f32.ge" = 0x60;
pub const @"f64.eq" = 0x61;
pub const @"f64.ne" = 0x62;
pub const @"f64.lt" = 0x63;
pub const @"f64.gt" = 0x64;
pub const @"f64.le" = 0x65;
pub const @"f64.ge" = 0x66;
pub const @"i32.clz" = 0x67;
pub const @"i32.ctz" = 0x68;
pub const @"i32.popcnt" = 0x69;
pub const @"i32.add" = 0x6a;
pub const @"i32.sub" = 0x6b;
pub const @"i32.mul" = 0x6c;
pub const @"i32.div_s" = 0x6d;
pub const @"i32.div_u" = 0x6e;
pub const @"i32.rem_s" = 0x6f;
pub const @"i32.rem_u" = 0x70;
pub const @"i32.and" = 0x71;
pub const @"i32.or" = 0x72;
pub const @"i32.xor" = 0x73;
pub const @"i32.shl" = 0x74;
pub const @"i32.shr_s" = 0x75;
pub const @"i32.shr_u" = 0x76;
pub const @"i32.rotl" = 0x77;
pub const @"i32.rotr" = 0x78;
pub const @"i64.clz" = 0x79;
pub const @"i64.ctz" = 0x7a;
pub const @"i64.popcnt" = 0x7b;
pub const @"i64.add" = 0x7c;
pub const @"i64.sub" = 0x7d;
pub const @"i64.mul" = 0x7e;
pub const @"i64.div_s" = 0x7f;
pub const @"i64.div_u" = 0x80;
pub const @"i64.rem_s" = 0x81;
pub const @"i64.rem_u" = 0x82;
pub const @"i64.and" = 0x83;
pub const @"i64.or" = 0x84;
pub const @"i64.xor" = 0x85;
pub const @"i64.shl" = 0x86;
pub const @"i64.shr_s" = 0x87;
pub const @"i64.shr_u" = 0x88;
pub const @"i64.rotl" = 0x89;
pub const @"i64.rotr" = 0x8a;
pub const @"f32.abs" = 0x8b;
pub const @"f32.neg" = 0x8c;
pub const @"f32.ceil" = 0x8d;
pub const @"f32.floor" = 0x8e;
pub const @"f32.trunc" = 0x8f;
pub const @"f32.nearest" = 0x90;
pub const @"f32.sqrt" = 0x91;
pub const @"f32.add" = 0x92;
pub const @"f32.sub" = 0x93;
pub const @"f32.mul" = 0x94;
pub const @"f32.div" = 0x95;
pub const @"f32.min" = 0x96;
pub const @"f32.max" = 0x97;
pub const @"f32.copysign" = 0x98;
pub const @"f64.abs" = 0x99;
pub const @"f64.neg" = 0x9a;
pub const @"f64.ceil" = 0x9b;
pub const @"f64.floor" = 0x9c;
pub const @"f64.trunc" = 0x9d;
pub const @"f64.nearest" = 0x9e;
pub const @"f64.sqrt" = 0x9f;
pub const @"f64.add" = 0xa0;
pub const @"f64.sub" = 0xa1;
pub const @"f64.mul" = 0xa2;
pub const @"f64.div" = 0xa3;
pub const @"f64.min" = 0xa4;
pub const @"f64.max" = 0xa5;
pub const @"f64.copysign" = 0xa6;
pub const @"i32.wrap_i64" = 0xa7;
pub const @"i32.trunc_f32_s" = 0xa8;
pub const @"i32.trunc_f32_u" = 0xa9;
pub const @"i32.trunc_f64_s" = 0xaa;
pub const @"i32.trunc_f64_u" = 0xab;
pub const @"i64.extend_i32_s" = 0xac;
pub const @"i64.extend_i32_u" = 0xad;
pub const @"i64.trunc_f32_s" = 0xae;
pub const @"i64.trunc_f32_u" = 0xaf;
pub const @"i64.trunc_f64_s" = 0xb0;
pub const @"i64.trunc_f64_u" = 0xb1;
pub const @"f32.convert_i32_s" = 0xb2;
pub const @"f32.convert_i32_u" = 0xb3;
pub const @"f32.convert_i64_s" = 0xb4;
pub const @"f32.convert_i64_u" = 0xb5;
pub const @"f32.demote_f64" = 0xb6;
pub const @"f64.convert_i32_s" = 0xb7;
pub const @"f64.convert_i32_u" = 0xb8;
pub const @"f64.convert_i64_s" = 0xb9;
pub const @"f64.convert_i64_u" = 0xba;
pub const @"f64.promote_f32" = 0xbb;
pub const @"i32.reinterpret_f32" = 0xbc;
pub const @"i64.reinterpret_f64" = 0xbd;
pub const @"f32.reinterpret_i32" = 0xbe;
pub const @"f64.reinterpret_i64" = 0xbf;
pub const @"i32.extend8_s" = 0xc0;
pub const @"i32.extend16_s" = 0xc1;
pub const @"i64.extend8_s" = 0xc2;
pub const @"i64.extend16_s" = 0xc3;
pub const @"i64.extend32_s" = 0xc4;
pub const @"ref.null" = 0xd0;
pub const @"ref.is_null" = 0xd1;
pub const @"ref.func" = 0xd2;

pub const general_prefix = 0xfc;
pub const @"i32.trunc_sat_f32_s" = 0x00;
pub const @"i32.trunc_sat_f32_u" = 0x01;
pub const @"i32.trunc_sat_f64_s" = 0x02;
pub const @"i32.trunc_sat_f64_u" = 0x03;
pub const @"i64.trunc_sat_f32_s" = 0x04;
pub const @"i64.trunc_sat_f32_u" = 0x05;
pub const @"i64.trunc_sat_f64_s" = 0x06;
pub const @"i64.trunc_sat_f64_u" = 0x07;
pub const @"memory.init" = 0x08;
pub const @"data.drop" = 0x09;
pub const @"memory.copy" = 0x0a;
pub const @"memory.fill" = 0x0b;
pub const @"table.init" = 0x0c;
pub const @"elem.drop" = 0x0d;
pub const @"table.copy" = 0x0e;
pub const @"table.grow" = 0x0f;
pub const @"table.size" = 0x10;
pub const @"table.fill" = 0x11;

pub const simd_prefix = 0xfd;
pub const @"v128.load" = 0x00;
pub const @"v128.load8x8_s" = 0x01;
pub const @"v128.load8x8_u" = 0x02;
pub const @"v128.load16x4_s" = 0x03;
pub const @"v128.load16x4_u" = 0x04;
pub const @"v128.load32x2_s" = 0x05;
pub const @"v128.load32x2_u" = 0x06;
pub const @"v128.load8_splat" = 0x07;
pub const @"v128.load16_splat" = 0x08;
pub const @"v128.load32_splat" = 0x09;
pub const @"v128.load64_splat" = 0x0a;
pub const @"v128.store" = 0x0b;
pub const @"v128.const" = 0x0c;
pub const @"i8x16.shuffle" = 0x0d;
pub const @"i8x16.swizzle" = 0x0e;
pub const @"i8x16.splat" = 0x0f;
pub const @"i16x8.splat" = 0x10;
pub const @"i32x4.splat" = 0x11;
pub const @"i64x2.splat" = 0x12;
pub const @"f32x4.splat" = 0x13;
pub const @"f64x2.splat" = 0x14;
pub const @"i8x16.extract_lane_s" = 0x15;
pub const @"i8x16.extract_lane_u" = 0x16;
pub const @"i8x16.replace_lane" = 0x17;
pub const @"i16x8.extract_lane_s" = 0x18;
pub const @"i16x8.extract_lane_u" = 0x19;
pub const @"i16x8.replace_lane" = 0x1a;
pub const @"i32x4.extract_lane" = 0x1b;
pub const @"i32x4.replace_lane" = 0x1c;
pub const @"i64x2.extract_lane" = 0x1d;
pub const @"i64x2.replace_lane" = 0x1e;
pub const @"f32x4.extract_lane" = 0x1f;
pub const @"f32x4.replace_lane" = 0x20;
pub const @"f64x2.extract_lane" = 0x21;
pub const @"f64x2.replace_lane" = 0x22;
pub const @"i8x16.eq" = 0x23;
pub const @"i8x16.ne" = 0x24;
pub const @"i8x16.lt_s" = 0x25;
pub const @"i8x16.lt_u" = 0x26;
pub const @"i8x16.gt_s" = 0x27;
pub const @"i8x16.gt_u" = 0x28;
pub const @"i8x16.le_s" = 0x29;
pub const @"i8x16.le_u" = 0x2a;
pub const @"i8x16.ge_s" = 0x2b;
pub const @"i8x16.ge_u" = 0x2c;
pub const @"i16x8.eq" = 0x2d;
pub const @"i16x8.ne" = 0x2e;
pub const @"i16x8.lt_s" = 0x2f;
pub const @"i16x8.lt_u" = 0x30;
pub const @"i16x8.gt_s" = 0x31;
pub const @"i16x8.gt_u" = 0x32;
pub const @"i16x8.le_s" = 0x33;
pub const @"i16x8.le_u" = 0x34;
pub const @"i16x8.ge_s" = 0x35;
pub const @"i16x8.ge_u" = 0x36;
pub const @"i32x4.eq" = 0x37;
pub const @"i32x4.ne" = 0x38;
pub const @"i32x4.lt_s" = 0x39;
pub const @"i32x4.lt_u" = 0x3a;
pub const @"i32x4.gt_s" = 0x3b;
pub const @"i32x4.gt_u" = 0x3c;
pub const @"i32x4.le_s" = 0x3d;
pub const @"i32x4.le_u" = 0x3e;
pub const @"i32x4.ge_s" = 0x3f;
pub const @"i32x4.ge_u" = 0x40;
pub const @"f32x4.eq" = 0x41;
pub const @"f32x4.ne" = 0x42;
pub const @"f32x4.lt" = 0x43;
pub const @"f32x4.gt" = 0x44;
pub const @"f32x4.le" = 0x45;
pub const @"f32x4.ge" = 0x46;
pub const @"f64x2.eq" = 0x47;
pub const @"f64x2.ne" = 0x48;
pub const @"f64x2.lt" = 0x49;
pub const @"f64x2.gt" = 0x4a;
pub const @"f64x2.le" = 0x4b;
pub const @"f64x2.ge" = 0x4c;
pub const @"v128.not" = 0x4d;
pub const @"v128.and" = 0x4e;
pub const @"v128.andnot" = 0x4f;
pub const @"v128.or" = 0x50;
pub const @"v128.xor" = 0x51;
pub const @"v128.bitselect" = 0x52;
pub const @"v128.any_true" = 0x53;
pub const @"v128.load8_lane" = 0x54;
pub const @"v128.load16_lane" = 0x55;
pub const @"v128.load32_lane" = 0x56;
pub const @"v128.load64_lane" = 0x57;
pub const @"v128.store8_lane" = 0x58;
pub const @"v128.store16_lane" = 0x59;
pub const @"v128.store32_lane" = 0x5a;
pub const @"v128.store64_lane" = 0x5b;
pub const @"v128.load32_zero" = 0x5c;
pub const @"v128.load64_zero" = 0x5d;
pub const @"f32x4.demote_f64x2_zero" = 0x5e;
pub const @"f64x2.promote_low_f32x4" = 0x5f;
pub const @"i8x16.abs" = 0x60;
pub const @"i8x16.neg" = 0x61;
pub const @"i8x16.popcnt" = 0x62;
pub const @"i8x16.all_true" = 0x63;
pub const @"i8x16.bitmask" = 0x64;
pub const @"i8x16.narrow_i16x8_s" = 0x65;
pub const @"i8x16.narrow_i16x8_u" = 0x66;
pub const @"i8x16.shl" = 0x6b;
pub const @"i8x16.shr_s" = 0x6c;
pub const @"i8x16.shr_u" = 0x6d;
pub const @"i8x16.add" = 0x6e;
pub const @"i8x16.add_sat_s" = 0x6f;
pub const @"i8x16.add_sat_u" = 0x70;
pub const @"i8x16.sub" = 0x71;
pub const @"i8x16.sub_sat_s" = 0x72;
pub const @"i8x16.sub_sat_u" = 0x73;
pub const @"i8x16.min_s" = 0x76;
pub const @"i8x16.min_u" = 0x77;
pub const @"i8x16.max_s" = 0x78;
pub const @"i8x16.max_u" = 0x79;
pub const @"i8x16.avgr_u" = 0x7b;
pub const @"i16x8.extadd_pairwise_i8x16_s" = 0x7c;
pub const @"i16x8.extadd_pairwise_i8x16_u" = 0x7d;
pub const @"i32x4.extadd_pairwise_i16x8_s" = 0x7e;
pub const @"i32x4.extadd_pairwise_i16x8_u" = 0x7f;
pub const @"i16x8.abs" = 0x80;
pub const @"i16x8.neg" = 0x81;
pub const @"i16x8.q15mulr_sat_s" = 0x82;
pub const @"i16x8.all_true" = 0x83;
pub const @"i16x8.bitmask" = 0x84;
pub const @"i16x8.narrow_i32x4_s" = 0x85;
pub const @"i16x8.narrow_i32x4_u" = 0x86;
pub const @"i16x8.extend_low_i8x16_s" = 0x87;
pub const @"i16x8.extend_high_i8x16_s" = 0x88;
pub const @"i16x8.extend_low_i8x16_u" = 0x89;
pub const @"i16x8.extend_high_i8x16_u" = 0x8a;
pub const @"i16x8.shl" = 0x8b;
pub const @"i16x8.shr_s" = 0x8c;
pub const @"i16x8.shr_u" = 0x8d;
pub const @"i16x8.add" = 0x8e;
pub const @"i16x8.add_sat_s" = 0x8f;
pub const @"i16x8.add_sat_u" = 0x90;
pub const @"i16x8.sub" = 0x91;
pub const @"i16x8.sub_sat_s" = 0x92;
pub const @"i16x8.sub_sat_u" = 0x93;
pub const @"i16x8.mul" = 0x95;
pub const @"i16x8.min_s" = 0x96;
pub const @"i16x8.min_u" = 0x97;
pub const @"i16x8.max_s" = 0x98;
pub const @"i16x8.max_u" = 0x99;
pub const @"i16x8.avgr_u" = 0x9b;
pub const @"i16x8.extmul_low_i8x16_s" = 0x9c;
pub const @"i16x8.extmul_high_i8x16_s" = 0x9d;
pub const @"i16x8.extmul_low_i8x16_u" = 0x9e;
pub const @"i16x8.extmul_high_i8x16_u" = 0x9f;
pub const @"i32x4.abs" = 0xa0;
pub const @"i32x4.neg" = 0xa1;
pub const @"i32x4.all_true" = 0xa3;
pub const @"i32x4.bitmask" = 0xa4;
pub const @"i32x4.extend_low_i16x8_s" = 0xa7;
pub const @"i32x4.extend_high_i16x8_s" = 0xa8;
pub const @"i32x4.extend_low_i16x8_u" = 0xa9;
pub const @"i32x4.extend_high_i16x8_u" = 0xaa;
pub const @"i32x4.shl" = 0xab;
pub const @"i32x4.shr_s" = 0xac;
pub const @"i32x4.shr_u" = 0xad;
pub const @"i32x4.add" = 0xae;
pub const @"i32x4.sub" = 0xb1;
pub const @"i32x4.mul" = 0xb5;
pub const @"i32x4.min_s" = 0xb6;
pub const @"i32x4.min_u" = 0xb7;
pub const @"i32x4.max_s" = 0xb8;
pub const @"i32x4.max_u" = 0xb9;
pub const @"i32x4.dot_i16x8_s" = 0xba;
pub const @"i32x4.extmul_low_i16x8_s" = 0xbc;
pub const @"i32x4.extmul_high_i16x8_s" = 0xbd;
pub const @"i32x4.extmul_low_i16x8_u" = 0xbe;
pub const @"i32x4.extmul_high_i16x8_u" = 0xbf;
pub const @"i64x2.abs" = 0xc0;
pub const @"i64x2.neg" = 0xc1;
pub const @"i64x2.all_true" = 0xc3;
pub const @"i64x2.bitmask" = 0xc4;
pub const @"i64x2.extend_low_i32x4_s" = 0xc7;
pub const @"i64x2.extend_high_i32x4_s" = 0xc8;
pub const @"i64x2.extend_low_i32x4_u" = 0xc9;
pub const @"i64x2.extend_high_i32x4_u" = 0xca;
pub const @"i64x2.shl" = 0xcb;
pub const @"i64x2.shr_s" = 0xcc;
pub const @"i64x2.shr_u" = 0xcd;
pub const @"i64x2.add" = 0xce;
pub const @"i64x2.sub" = 0xd1;
pub const @"i64x2.mul" = 0xd5;
pub const @"i64x2.eq" = 0xd6;
pub const @"i64x2.ne" = 0xd7;
pub const @"i64x2.lt_s" = 0xd8;
pub const @"i64x2.gt_s" = 0xd9;
pub const @"i64x2.le_s" = 0xda;
pub const @"i64x2.ge_s" = 0xdb;
pub const @"i64x2.extmul_low_i32x4_s" = 0xdc;
pub const @"i64x2.extmul_high_i32x4_s" = 0xdd;
pub const @"i64x2.extmul_low_i32x4_u" = 0xde;
pub const @"i64x2.extmul_high_i32x4_u" = 0xdf;
pub const @"f32x4.ceil" = 0x67;
pub const @"f32x4.floor" = 0x68;
pub const @"f32x4.trunc" = 0x69;
pub const @"f32x4.nearest" = 0x6a;
pub const @"f64x2.ceil" = 0x74;
pub const @"f64x2.floor" = 0x75;
pub const @"f64x2.trunc" = 0x7a;
pub const @"f64x2.nearest" = 0x94;
pub const @"f32x4.abs" = 0xe0;
pub const @"f32x4.neg" = 0xe1;
pub const @"f32x4.sqrt" = 0xe3;
pub const @"f32x4.add" = 0xe4;
pub const @"f32x4.sub" = 0xe5;
pub const @"f32x4.mul" = 0xe6;
pub const @"f32x4.div" = 0xe7;
pub const @"f32x4.min" = 0xe8;
pub const @"f32x4.max" = 0xe9;
pub const @"f32x4.pmin" = 0xea;
pub const @"f32x4.pmax" = 0xeb;
pub const @"f64x2.abs" = 0xec;
pub const @"f64x2.neg" = 0xed;
pub const @"f64x2.sqrt" = 0xef;
pub const @"f64x2.add" = 0xf0;
pub const @"f64x2.sub" = 0xf1;
pub const @"f64x2.mul" = 0xf2;
pub const @"f64x2.div" = 0xf3;
pub const @"f64x2.min" = 0xf4;
pub const @"f64x2.max" = 0xf5;
pub const @"f64x2.pmin" = 0xf6;
pub const @"f64x2.pmax" = 0xf7;
pub const @"i32x4.trunc_sat_f32x4_s" = 0xf8;
pub const @"i32x4.trunc_sat_f32x4_u" = 0xf9;
pub const @"f32x4.convert_i32x4_s" = 0xfa;
pub const @"f32x4.convert_i32x4_u" = 0xfb;
pub const @"i32x4.trunc_sat_f64x2_s_zero" = 0xfc;
pub const @"i32x4.trunc_sat_f64x2_u_zero" = 0xfd;
pub const @"f64x2.convert_low_i32x4_s" = 0xfe;
pub const @"f64x2.convert_low_i32x4_u" = 0xff;
pub const @"i8x16.relaxed_swizzle" = 0x100;
pub const @"i32x4.relaxed_trunc_f32x4_s" = 0x101;
pub const @"i32x4.relaxed_trunc_f32x4_u" = 0x102;
pub const @"i32x4.relaxed_trunc_f64x2_s_zero" = 0x103;
pub const @"i32x4.relaxed_trunc_f64x2_u_zero" = 0x104;
pub const @"f32x4.relaxed_madd" = 0x105;
pub const @"f32x4.relaxed_nmadd" = 0x106;
pub const @"f64x2.relaxed_madd" = 0x107;
pub const @"f64x2.relaxed_nmadd" = 0x108;
pub const @"i8x16.relaxed_laneselect" = 0x109;
pub const @"i16x8.relaxed_laneselect" = 0x10a;
pub const @"i32x4.relaxed_laneselect" = 0x10b;
pub const @"i64x2.relaxed_laneselect" = 0x10c;
pub const @"f32x4.relaxed_min" = 0x10d;
pub const @"f32x4.relaxed_max" = 0x10e;
pub const @"f64x2.relaxed_min" = 0x10f;
pub const @"f64x2.relaxed_max" = 0x110;
pub const @"i16x8.relaxed_q15mulr_s" = 0x111;
pub const @"i16x8.dot_i8x16_i7x16_s" = 0x112;
pub const @"i32x4.dot_i8x16_i7x16_add_s" = 0x113;

pub const thread_prefix = 0xfe;
pub const @"memory.atomic.notify" = 0x00;
pub const @"memory.atomic.wait32" = 0x01;
pub const @"memory.atomic.wait64" = 0x02;
pub const @"atomic.fence" = 0x03;
pub const @"i32.atomic.load" = 0x10;
pub const @"i64.atomic.load" = 0x11;
pub const @"i32.atomic.load8_u" = 0x12;
pub const @"i32.atomic.load16_u" = 0x13;
pub const @"i64.atomic.load8_u" = 0x14;
pub const @"i64.atomic.load16_u" = 0x15;
pub const @"i64.atomic.load32_u" = 0x16;
pub const @"i32.atomic.store" = 0x17;
pub const @"i64.atomic.store" = 0x18;
pub const @"i32.atomic.store8" = 0x19;
pub const @"i32.atomic.store16" = 0x1a;
pub const @"i64.atomic.store8" = 0x1b;
pub const @"i64.atomic.store16" = 0x1c;
pub const @"i64.atomic.store32" = 0x1d;
pub const @"i32.atomic.rmw.add" = 0x1e;
pub const @"i64.atomic.rmw.add" = 0x1f;
pub const @"i32.atomic.rmw8.add_u" = 0x20;
pub const @"i32.atomic.rmw16.add_u" = 0x21;
pub const @"i64.atomic.rmw8.add_u" = 0x22;
pub const @"i64.atomic.rmw16.add_u" = 0x23;
pub const @"i64.atomic.rmw32.add_u" = 0x24;
pub const @"i32.atomic.rmw.sub" = 0x25;
pub const @"i64.atomic.rmw.sub" = 0x26;
pub const @"i32.atomic.rmw8.sub_u" = 0x27;
pub const @"i32.atomic.rmw16.sub_u" = 0x28;
pub const @"i64.atomic.rmw8.sub_u" = 0x29;
pub const @"i64.atomic.rmw16.sub_u" = 0x2a;
pub const @"i64.atomic.rmw32.sub_u" = 0x2b;
pub const @"i32.atomic.rmw.and" = 0x2c;
pub const @"i64.atomic.rmw.and" = 0x2d;
pub const @"i32.atomic.rmw8.and_u" = 0x2e;
pub const @"i32.atomic.rmw16.and_u" = 0x2f;
pub const @"i64.atomic.rmw8.and_u" = 0x30;
pub const @"i64.atomic.rmw16.and_u" = 0x31;
pub const @"i64.atomic.rmw32.and_u" = 0x32;
pub const @"i32.atomic.rmw.or" = 0x33;
pub const @"i64.atomic.rmw.or" = 0x34;
pub const @"i32.atomic.rmw8.or_u" = 0x35;
pub const @"i32.atomic.rmw16.or_u" = 0x36;
pub const @"i64.atomic.rmw8.or_u" = 0x37;
pub const @"i64.atomic.rmw16.or_u" = 0x38;
pub const @"i64.atomic.rmw32.or_u" = 0x39;
pub const @"i32.atomic.rmw.xor" = 0x3a;
pub const @"i64.atomic.rmw.xor" = 0x3b;
pub const @"i32.atomic.rmw8.xor_u" = 0x3c;
pub const @"i32.atomic.rmw16.xor_u" = 0x3d;
pub const @"i64.atomic.rmw8.xor_u" = 0x3e;
pub const @"i64.atomic.rmw16.xor_u" = 0x3f;
pub const @"i64.atomic.rmw32.xor_u" = 0x40;
pub const @"i32.atomic.rmw.xchg" = 0x41;
pub const @"i64.atomic.rmw.xchg" = 0x42;
pub const @"i32.atomic.rmw8.xchg_u" = 0x43;
pub const @"i32.atomic.rmw16.xchg_u" = 0x44;
pub const @"i64.atomic.rmw8.xchg_u" = 0x45;
pub const @"i64.atomic.rmw16.xchg_u" = 0x46;
pub const @"i64.atomic.rmw32.xchg_u" = 0x47;
pub const @"i32.atomic.rmw.cmpxchg" = 0x48;
pub const @"i64.atomic.rmw.cmpxchg" = 0x49;
pub const @"i32.atomic.rmw8.cmpxchg_u" = 0x4a;
pub const @"i32.atomic.rmw16.cmpxchg_u" = 0x4b;
pub const @"i64.atomic.rmw8.cmpxchg_u" = 0x4c;
pub const @"i64.atomic.rmw16.cmpxchg_u" = 0x4d;
pub const @"i64.atomic.rmw32.cmpxchg_u" = 0x4e;

test "ref all decls" {
    @import("std").testing.refAllDecls(@This());
}
