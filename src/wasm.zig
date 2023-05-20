const std = @import("std");

pub const Decoder = @import("wasm/Decoder.zig");

pub const NumberType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
};

pub const VectorType = enum(u8) {
    v128 = 0x7b,
};

pub const ReferenceType = enum(u8) {
    funcref = 0x70,
    externref = 0x6f,
};

pub const ValueType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    v128 = 0x7b,
    funcref = 0x70,
    externref = 0x6f,
};

pub const ResultType = []const ValueType;

pub const FunctionType = struct {
    parameters: ResultType,
    results: ResultType,
};

pub const Limits = struct {
    min: u32,
    max: ?u32 = null,
};

pub const MemoryType = struct {
    limits: Limits,
};

pub const TableType = struct {
    element_type: ReferenceType,
    limits: Limits,
};

pub const Mutability = enum(u8) {
    @"const" = 0x00,
    @"var" = 0x01,
};

pub const GlobalType = struct {
    value_type: ValueType,
    mutability: Mutability,
};

pub const MemArg = struct {
    alignment: u32,
    offset: u32,
};

pub const Name = []const u8;

pub fn visitModule(module_data: []const u8, visitor: anytype) !void {
    var decoder = Decoder.init(module_data);

    const magic = try decoder.nextBytes(4);
    if (!std.meta.eql(magic.*, [_]u8{ 0x00, 0x61, 0x73, 0x6d }))
        return error.NotABinaryWasmModule;

    const version = try decoder.nextBytes(4);
    if (!std.meta.eql(version.*, [_]u8{ 0x01, 0x00, 0x00, 0x00 }))
        return error.UnsupportedBinaryFormatVersion;

    while (decoder.hasRemainingData()) {
        const section_id = try decoder.nextByte();
        const section_len = try decoder.nextInt(u32);
        const section_data = try decoder.nextBytes(section_len);
        try switch (section_id) {
            0 => visitor.customSection(section_data),
            1 => visitor.typeSection(section_data),
            2 => visitor.importSection(section_data),
            3 => visitor.functionSection(section_data),
            4 => visitor.tableSection(section_data),
            5 => visitor.memorySection(section_data),
            6 => visitor.globalSection(section_data),
            7 => visitor.exportSection(section_data),
            8 => visitor.startSection(section_data),
            9 => visitor.elementSection(section_data),
            10 => visitor.codeSection(section_data),
            11 => visitor.dataSection(section_data),
            12 => visitor.dataCountSection(section_data),
            else => return error.InvalidSectionId,
        };
    }
}
