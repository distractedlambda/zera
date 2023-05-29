pub const @"unreachable" = 0x00;
pub const nop = 0x01;
pub const block = 0x02;
pub const loop = 0x03;
pub const @"if" = 0x04;
pub const @"else" = 0x05;
pub const end = 0x0b;
pub const br = 0x0c;
pub const br_if = 0x0d;
pub const br_table = 0x0e;
pub const @"return" = 0x0f;
pub const call = 0x10;
pub const call_indirect = 0x11;
pub const @"ref.null" = 0xd0;
pub const @"ref.is_null" = 0xd1;
pub const @"ref.func" = 0xd2;
pub const drop = 0x1a;
pub const select = 0x1b;
pub const @"select t*" = 0x1c;
pub const @"local.get" = 0x20;
pub const @"local.set" = 0x21;
pub const @"local.tee" = 0x22;
pub const @"global.get" = 0x23;
pub const @"global.set" = 0x24;
pub const @"i32.const" = 0x41;
pub const @"i64.const" = 0x42;
pub const @"f32.const" = 0x43;
pub const @"f64.const" = 0x44;

test "ref all decls" {
    @import("std").testing.refAllDecls(@This());
}
