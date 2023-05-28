const std = @import("std");

pub const TypeIndex = packed struct(u32) { value: u32 };
pub const FunctionIndex = packed struct(u32) { value: u32 };
pub const TableIndex = packed struct(u32) { value: u32 };
pub const MemoryIndex = packed struct(u32) { value: u32 };
pub const GlobalIndex = packed struct(u32) { value: u32 };
pub const ElementSegmentIndex = packed struct(u32) { value: u32 };
pub const DataSegmentIndex = packed struct(u32) { value: u32 };
pub const LocalIndex = packed struct(u32) { value: u32 };
pub const LabelIndex = packed struct(u32) { value: u32 };
pub const LaneIndex = u8;

pub const SimpleInstruction = enum(u8) {
    @"unreachable" = 0x00,
    nop,
    @"else" = 0x05,
    end = 0x0b,
    @"return" = 0x0f,
    drop = 0x1a,
    i32_eqz = 0x45,
    i32_eq,
    i32_ne,
    i32_lt_s,
    i32_lt_u,
    i32_gt_s,
    i32_gt_u,
    i32_le_s,
    i32_le_u,
    i32_ge_s,
    i32_ge_u,
    i64_eqz,
    i64_eq,
    i64_ne,
    i64_lt_s,
    i64_lt_u,
    i64_gt_s,
    i64_gt_u,
    i64_le_s,
    i64_le_u,
    i64_ge_s,
    i64_ge_u,
    f32_eq,
    f32_ne,
    f32_lt,
    f32_gt,
    f32_le,
    f32_ge,
    f64_eq,
    f64_ne,
    f64_lt,
    f64_gt,
    f64_le,
    f64_ge,
    i32_clz,
    i32_ctz,
    i32_popcnt,
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_s,
    i32_div_u,
    i32_rem_s,
    i32_rem_u,
    i32_and,
    i32_or,
    i32_xor,
    i32_shl,
    i32_shr_s,
    i32_shr_u,
    i32_rotl,
    i32_rotr,
    i64_clz,
    i64_ctz,
    i64_popcnt,
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_s,
    i64_div_u,
    i64_rem_s,
    i64_rem_u,
    i64_and,
    i64_or,
    i64_xor,
    i64_shl,
    i64_shr_s,
    i64_shr_u,
    i64_rotl,
    i64_rotr,
    f32_abs,
    f32_neg,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f32_sqrt,
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_copysign,
    f64_abs,
    f64_neg,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    f64_sqrt,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_copysign,
    i32_wrap_i64,
    i32_trunc_f32_s,
    i32_trunc_f32_u,
    i32_trunc_f64_s,
    i32_trunc_f64_u,
    i64_extend_i32_s,
    i64_extend_i32_u,
    i64_trunc_f32_s,
    i64_trunc_f32_u,
    i64_trunc_f64_s,
    i64_trunc_f64_u,
    f32_convert_i32_s,
    f32_convert_i32_u,
    f32_convert_i64_s,
    f32_convert_i64_u,
    f32_demote_f64,
    f64_convert_i32_s,
    f64_convert_i32_u,
    f64_convert_i64_s,
    f64_convert_i64_u,
    f64_promote_f32,
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    f32_reinterpret_i32,
    f64_reinterpret_i64,
    i32_extend8_s,
    i32_extend16_s,
    i64_extend8_s,
    i64_extend16_s,
    i64_extend32_s,
    ref_is_null = 0xd1,
};

pub const BlockTypeInstruction = enum(u8) {
    block = 0x02,
    loop,
    @"if",
};

pub const IndexInstruction = enum(u8) {
    br = 0x0c,
    br_if,
    call = 0x10,
    local_get = 0x20,
    local_set,
    local_tee,
    global_get,
    global_set,
    table_get,
    table_set,
    memory_size = 0x3f,
    memory_grow,
    ref_func = 0xd2,
};

pub const MemoryInstruction = enum(u8) {
    i32_load = 0x28,
    i64_load,
    f32_load,
    f64_load,
    i32_load8_s,
    i32_load8_u,
    i32_load16_s,
    i32_load16_u,
    i64_load8_s,
    i64_load8_u,
    i64_load16_s,
    i64_load16_u,
    i64_load32_s,
    i64_load32_u,
    i32_store,
    i64_store,
    f32_store,
    f64_store,
    i32_store8,
    i32_store16,
    i64_store8,
    i64_store16,
    i64_store32,
};

pub const ExtendedInstruction = enum(u32) {
    i32_trunc_sat_f32_s = 0,
    i32_trunc_sat_f32_u,
    i32_trunc_sat_f64_s,
    i32_trunc_sat_f64_u,
    i64_trunc_sat_f32_s,
    i64_trunc_sat_f32_u,
    i64_trunc_sat_f64_s,
    i64_trunc_sat_f64_u,
};

pub const ExtendedIndexInstruction = enum(u32) {
    data_drop = 9,
    memory_fill = 11,
    elem_drop = 13,
    table_grow = 15,
    table_size,
    table_fill,
};

pub const ExtendedDualIndexInstruction = enum(u32) {
    memory_init = 8,
    memory_copy = 10,
    table_init = 12,
    table_copy = 14,
};

pub const VectorInstruction = enum(u32) {
    i8x16_swizzle = 14,
    i8x16_splat,
    i16x8_splat,
    i32x4_splat,
    i64x2_splat,
    f32x4_splat,
    f64x2_splat,
    i8x16_eq = 35,
    i8x16_ne,
    i8x16_lt_s,
    i8x16_lt_u,
    i8x16_gt_s,
    i8x16_gt_u,
    i8x16_le_s,
    i8x16_le_u,
    i8x16_ge_s,
    i8x16_ge_u,
    i16x8_eq,
    i16x8_ne,
    i16x8_lt_s,
    i16x8_lt_u,
    i16x8_gt_s,
    i16x8_gt_u,
    i16x8_le_s,
    i16x8_le_u,
    i16x8_ge_s,
    i16x8_ge_u,
    i32x4_eq,
    i32x4_ne,
    i32x4_lt_s,
    i32x4_lt_u,
    i32x4_gt_s,
    i32x4_gt_u,
    i32x4_le_s,
    i32x4_le_u,
    i32x4_ge_s,
    i32x4_ge_u,
    f32x4_eq,
    f32x4_ne,
    f32x4_lt,
    f32x4_gt,
    f32x4_le,
    f32x4_ge,
    f64x2_eq,
    f64x2_ne,
    f64x2_lt,
    f64x2_gt,
    f64x2_le,
    f64x2_ge,
    v128_not,
    v128_and,
    v128_andnot,
    v128_or,
    v128_xor,
    v128_bitselect,
    v128_any_true,
    f32x4_demote_f64x2_zero = 94,
    f64x2_promote_low_f32x4,
    i8x16_abs,
    i8x16_neg,
    i8x16_popcnt,
    i8x16_all_true,
    i8x16_bitmask,
    i8x16_narrow_i16x8_s,
    i8x16_narrow_i16x8_u,
    f32x4_ceil,
    f32x4_floor,
    f32x4_trunc,
    f32x4_nearest,
    i8x16_shl,
    i8x16_shr_s,
    i8x16_shr_u,
    i8x16_add,
    i8x16_add_sat_s,
    i8x16_add_sat_u,
    i8x16_sub,
    i8x16_sub_sat_s,
    i8x16_sub_sat_u,
    f64x2_ceil,
    f64x2_floor,
    i8x16_min_s,
    i8x16_min_u,
    i8x16_max_s,
    i8x16_max_u,
    f64x2_trunc,
    i8x16_avgr_u,
    i16x8_extadd_pairwise_i8x16_s,
    i16x8_extadd_pairwise_i8x16_u,
    i32x4_extadd_pairwise_i16x8_s,
    i32x4_extadd_pairwise_i16x8_u,
    i16x8_abs,
    i16x8_neg,
    i16x8_q15mulr_sat_s,
    i16x8_all_true,
    i16x8_bitmask,
    i16x8_narrow_i32x4_s,
    i16x8_narrow_i32x4_u,
    i16x8_extend_low_i8x16_s,
    i16x8_extend_high_i8x16_s,
    i16x8_extend_low_i8x16_u,
    i16x8_extend_high_i8x16_u,
    i16x8_shl,
    i16x8_shr_s,
    i16x8_shr_u,
    i16x8_add,
    i16x8_add_sat_s,
    i16x8_add_sat_u,
    i16x8_sub,
    i16x8_sub_sat_s,
    i16x8_sub_sat_u,
    f64x2_nearest,
    i16x8_mul,
    i16x8_min_s,
    i16x8_min_u,
    i16x8_max_s,
    i16x8_max_u,
    i16x8_avgr_u = 155,
    i16x8_extmul_low_i8x16_s,
    i16x8_extmul_high_i8x16_s,
    i16x8_extmul_low_i8x16_u,
    i16x8_extmul_high_i8x16_u,
    i32x4_abs,
    i32x4_neg,
    i32x4_all_true = 163,
    i32x4_bitmask,
    i32x4_extend_low_i16x8_s = 167,
    i32x4_extend_high_i16x8_s,
    i32x4_extend_low_i16x8_u,
    i32x4_extend_high_i16x8_u,
    i32x4_shl,
    i32x4_shr_s,
    i32x4_shr_u,
    i32x4_add,
    i32x4_sub = 177,
    i32x4_mul = 181,
    i32x4_min_s,
    i32x4_min_u,
    i32x4_max_s,
    i32x4_max_u,
    i32x4_dot_i16x8_s,
    i32x4_extmul_low_i16x8_s = 188,
    i32x4_extmul_high_i16x8_s,
    i32x4_extmul_low_i16x8_u,
    i32x4_extmul_high_i16x8_u,
    i64x2_abs,
    i64x2_neg,
    i64x2_all_true = 195,
    i64x2_bitmask,
    i64x2_extend_low_i32x4_s = 199,
    i64x2_extend_high_i32x4_s,
    i64x2_extend_low_i32x4_u,
    i64x2_extend_high_i32x4_u,
    i64x2_shl,
    i64x2_shr_s,
    i64x2_shr_u,
    i64x2_add,
    i64x2_sub = 209,
    i64x2_mul = 213,
    i64x2_eq,
    i64x2_ne,
    i64x2_lt_s,
    i64x2_gt_s,
    i64x2_le_s,
    i64x2_ge_s,
    i64x2_extmul_low_i32x4_s,
    i64x2_extmul_high_i32x4_s,
    i64x2_extmul_low_i32x4_u,
    i64x2_extmul_high_i32x4_u,
    f32x4_abs,
    f32x4_neg,
    f32x4_sqrt,
    f32x4_add,
    f32x4_sub,
    f32x4_mul,
    f32x4_div,
    f32x4_min,
    f32x4_max,
    f32x4_pmin,
    f32x4_pmax,
    f64x2_abs,
    f64x2_neg,
    f64x2_sqrt = 239,
    f64x2_add,
    f64x2_sub,
    f64x2_mul,
    f64x2_div,
    f64x2_min,
    f64x2_max,
    f64x2_pmin,
    f64x2_pmax,
    i32x4_trunc_sat_f32x4_s,
    i32x4_trunc_sat_f32x4_u,
    f32x4_convert_i32x4_s,
    f32x4_convert_i32x4_u,
    i32x4_trunc_sat_f64x2_s_zero,
    i32x4_trunc_sat_f64x2_u_zero,
    f64x2_convert_low_i32x4_s,
    f64x2_convert_low_i32x4_u,
};

pub const Vector16ByteImmediateInstruction = enum(u32) {
    v128_const = 12,
    i8x16_shuffle,
};

pub const VectorMemoryInstruction = enum(u32) {
    v128_load = 0,
    v128_load_8x8_s,
    v128_load_8x8_u,
    v128_load_16x4_s,
    v128_load_16x4_u,
    v128_load_32x2_s,
    v128_load_32x2_u,
    v128_load8_splat,
    v128_load16_splat,
    v128_load32_splat,
    v128_load64_splat,
    v128_store,
    v128_load32_zero = 92,
    v128_load64_zero,
};

pub const VectorLaneInstruction = enum(u32) {
    i8x16_extract_lane_s = 21,
    i8x16_extract_lane_u,
    i8x16_replace_lane,
    i16x8_extract_lane_s,
    i16x8_extract_lane_u,
    i16x8_replace_lane,
    i32x4_extract_lane,
    i32x4_replace_lane,
    i64x2_extract_lane,
    i64x2_replace_lane,
    f32x4_extract_lane,
    f32x4_replace_lane,
    f64x2_extract_lane,
    f64x2_replace_lane,
};

pub const VectorMemoryLaneInstruction = enum(u32) {
    v128_load8_lane = 84,
    v128_load16_lane,
    v128_load32_lane,
    v128_load64_lane,
    v128_store8_lane,
    v128_store16_lane,
    v128_store32_lane,
    v128_store64_lane,
};

pub const NumericType = enum(u8) {
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

pub const MemoryArgument = struct {
    alignment: u32,
    offset: u32,
};

pub const Name = []const u8;

pub const ImportName = struct {
    module: []const u8,
    name: []const u8,
};

pub const ImportedFunction = struct {
    name: ImportName,
    type: TypeIndex,
};

pub const ImportedTable = struct {
    name: ImportName,
    type: TableType,
};

pub const ImportedMemory = struct {
    name: ImportName,
    type: MemoryType,
};

pub const ImportedGlobal = struct {
    name: ImportName,
    type: GlobalType,
};

pub const ConstantExpression = union(enum) {
    i32_const: i32,
    i64_const: i64,
    f32_const: u32,
    f64_const: u64,
    ref_null: void,
    ref_func: FunctionIndex,
    global_get: GlobalIndex,
};

pub const I32ConstantExpression = union(enum) {
    i32_const: i32,
    global_get: GlobalIndex,
};

pub const FuncrefConstantExpression = union(enum) {
    ref_null: void,
    ref_func: FunctionIndex,
    global_get: GlobalIndex,
};

pub const ExternrefConstantExpression = union(enum) {
    ref_null: void,
    global_get: GlobalIndex,
};

pub const BlockType = union(enum) {
    immediate: ?ValueType,
    indexed: FunctionIndex,
};

pub const Global = struct {
    type: GlobalType,
    initial_value: ConstantExpression,
};

pub const ExportDesc = union(enum) {
    function: FunctionIndex,
    table: TableIndex,
    memory: MemoryIndex,
    global: GlobalIndex,
};

pub const Export = struct {
    name: Name,
    desc: ExportDesc,
};

pub const ElementSegmentKind = enum(u8) {
    funcref = 0x00,
};

pub const ElementSegment = struct {
    mode: Mode,
    init: Init,

    pub const Mode = union(enum) {
        active: Active,
        passive: void,
        declarative: void,

        pub const Active = struct {
            table: TableIndex,
            offset: I32ConstantExpression,
        };
    };

    pub const Init = union(enum) {
        funcrefs: []const FunctionIndex,
        funcref_exprs: []const FuncrefConstantExpression,
        externref_exprs: []const ExternrefConstantExpression,
    };
};

pub const DataSegment = struct {
    active: ?struct { MemoryIndex, u32 },
    contents: []const u8,
};

pub const Decoder = struct {
    data: []const u8,

    pub fn init(data: []const u8) @This() {
        return .{ .data = data };
    }

    pub fn atEnd(self: @This()) bool {
        return self.data.len == 0;
    }

    pub fn nextByte(self: *@This()) !u8 {
        if (self.data.len == 0) return error.UnexpectedEndOfData;
        defer self.data = self.data[1..];
        return self.data[0];
    }

    pub fn nextBytes(self: *@This(), len: anytype) !(switch (@typeInfo(@TypeOf(len))) {
        .ComptimeInt => *const [len]u8,

        .Int => |info| if (info.signedness != .unsigned)
            @compileError("signed lengths are not supported")
        else
            []const u8,

        else => @compileError("unsupported len type: " ++ @typeName(@TypeOf(len))),
    }) {
        if (self.data.len < len) return error.UnexpectedEndOfData;
        defer self.data = self.data[len..];
        return self.data[0..len];
    }

    pub fn remainder(self: *@This()) []const u8 {
        defer self.data = self.data[self.data.len..];
        return self.data;
    }

    pub fn nextInt(self: *@This(), comptime T: type) !T {
        const max_bytes = std.math.divCeil(comptime_int, @bitSizeOf(T), 7) catch unreachable;

        var accum: T = 0;

        inline for (0..max_bytes) |i| {
            const b = try self.nextByte();

            const remaining_bits = @bitSizeOf(T) - i * 7;
            if (comptime remaining_bits < 7) {
                if (b > std.math.maxInt(std.meta.Int(.unsigned, remaining_bits)))
                    return error.Overflow;

                return accum | (@as(
                    T,
                    @bitCast(
                        std.meta.Int(@typeInfo(T).Int.signedness, remaining_bits),
                        @truncate(std.meta.Int(.unsigned, remaining_bits), b),
                    ),
                ) << (i * 7));
            }

            if (b <= std.math.maxInt(u7))
                return accum | (@as(
                    T,
                    @bitCast(
                        std.meta.Int(@typeInfo(T).Int.signedness, 7),
                        @truncate(u7, b),
                    ),
                ) << (i * 7))
            else
                accum |= @as(T, @truncate(u7, b)) << (i * 7);
        }

        return error.Overflow;
    }

    pub fn nextFixedWidth(self: *@This(), comptime T: type) !T {
        return @bitCast(
            T,
            std.mem.readIntLittle(
                std.meta.Int(.unsigned, @bitSizeOf(T)),
                try self.nextBytes(comptime @divExact(@bitSizeOf(T), 8)),
            ),
        );
    }

    pub fn nextName(self: *@This()) !Name {
        return try self.nextBytes(try self.nextInt(u32));
    }

    pub fn nextNumericType(self: *@This()) !NumericType {
        return std.meta.intToEnum(NumericType, try self.nextByte()) catch
            error.UnsupportedNumberType;
    }

    pub fn nextVectorType(self: *@This()) !VectorType {
        return std.meta.intToEnum(VectorType, try self.nextByte()) catch
            error.UnsupportedVectorType;
    }

    pub fn nextReferenceType(self: *@This()) !ReferenceType {
        return std.meta.intToEnum(ReferenceType, try self.nextByte()) catch
            error.UnsupportedReferenceType;
    }

    pub fn nextValueType(self: *@This()) !ValueType {
        return std.meta.intToEnum(ValueType, try self.nextByte()) catch
            error.UnsupportedValueType;
    }

    pub fn nextResultType(self: *@This()) !ResultType {
        const len = try self.nextInt(u32);
        const types = try self.nextBytes(len);

        for (types) |t|
            _ = std.meta.intToEnum(ValueType, t) catch
                return error.UnsupportedValueType;

        return std.mem.bytesAsSlice(ValueType, types);
    }

    pub fn nextFunctionType(self: *@This()) !FunctionType {
        if (try self.nextByte() != 0x60)
            return error.UnsupportedFunctionType;

        return .{
            .parameters = try self.nextResultType(),
            .results = try self.nextResultType(),
        };
    }

    pub fn nextLimits(self: *@This()) !Limits {
        return switch (try self.nextByte()) {
            0x00 => .{ .min = try self.nextInt(u32) },
            0x01 => .{ .min = try self.nextInt(u32), .max = try self.nextInt(u32) },
            else => error.UnsupportedLimits,
        };
    }

    pub fn nextMemoryType(self: *@This()) !MemoryType {
        return .{ .limits = try self.nextLimits() };
    }

    pub fn nextTableType(self: *@This()) !TableType {
        return .{
            .element_type = try self.nextReferenceType(),
            .limits = try self.nextLimits(),
        };
    }

    pub fn nextMutability(self: *@This()) !Mutability {
        return std.meta.intToEnum(Mutability, try self.nextByte()) catch
            error.UnsupportedMutability;
    }

    pub fn nextElementSegmentKind(self: *@This()) !ElementSegmentKind {
        return std.meta.intToEnum(ElementSegmentKind, try self.nextByte()) catch
            error.UnsupportedElemKind;
    }

    pub fn nextGlobalType(self: *@This()) !GlobalType {
        return .{
            .value_type = try self.nextValueType(),
            .mutability = try self.nextMutability(),
        };
    }

    pub fn nextInstruction(self: *@This(), visitor: anytype) !void {
        switch (try self.nextByte()) {
            0x0e => {
                const len = try self.nextInt(u32);
                const table_visitor = try visitor.visitBrTable(len);
                for (0..len) |i| try table_visitor.visitCase(i, try self.nextInt(u32));
                try table_visitor.visitDefault(try self.nextInt(u32));
            },

            0x1b => {
                try visitor.visitSelect(null);
            },

            0x1c => {
                try visitor.visitSelect(switch (self.nextInt(u32)) {
                    0 => null,
                    1 => try self.nextValueType(),
                    else => return error.UnsupportedInstruction,
                });
            },

            0x41 => {
                try visitor.visitI32Const(try self.nextInt(i32));
            },

            0x42 => {
                try visitor.visitI64Const(try self.nextInt(i64));
            },

            0x43 => {
                try visitor.visitF32Const(try self.nextFloat(f32));
            },

            0x44 => {
                try visitor.visitF64Const(try self.nextFloat(f64));
            },

            0x00,
            0x01,
            0x05,
            0x0b,
            0x0f,
            0x1a,
            0x45...0xc4,
            0xd1,
            => |opcode| {
                try visitor.visitSimpleInstruction(@intToEnum(SimpleInstruction, opcode));
            },

            0x02...0x04 => |opcode| {
                try visitor.visitBlockTypeInstruction(
                    @intToEnum(BlockTypeInstruction, opcode),
                    try self.nextBlockType(),
                );
            },

            0x0c,
            0x0d,
            0x10,
            0x20...0x26,
            0x3f,
            0x40,
            0xd2,
            => |opcode| {
                try visitor.visitIndexInstruction(
                    @intToEnum(IndexInstruction, opcode),
                    try self.nextInt(u32),
                );
            },

            0x28...0x3e => |opcode| {
                try visitor.visitMemoryInstruction(
                    @intToEnum(MemoryInstruction, opcode),
                    try self.nextMemoryArgument(),
                );
            },

            0xfc => switch (try self.nextInt(u32)) {
                0...7 => |opcode| {
                    try visitor.extendedInstruction(@intToEnum(ExtendedInstruction, opcode));
                },

                9, 11, 13, 15...17 => |opcode| {
                    try visitor.visitExtendedIndexInstruction(
                        @intToEnum(ExtendedIndexInstruction, opcode),
                        try self.nextInt(u32),
                    );
                },

                8, 10, 12, 14 => |opcode| {
                    try visitor.visitExtendedDualIndexInstruction(
                        @intToEnum(ExtendedDualIndexInstruction, opcode),
                        try self.nextInt(u32),
                        try self.nextInt(u32),
                    );
                },

                else => return error.UnsupportedInstruction,
            },

            0xfd => switch (try self.nextInt(u32)) {
                14...20,
                35...64,
                94...153,
                155...161,
                163,
                164,
                167...174,
                177,
                181...186,
                188...193,
                195,
                196,
                199...206,
                209,
                213...237,
                239...255,
                => |opcode| {
                    try visitor.visitVectorInstruction(@intToEnum(VectorInstruction, opcode));
                },

                12, 13 => |opcode| {
                    try visitor.visitVector16ByteImmediateInstruction(
                        @intToEnum(Vector16ByteImmediateInstruction, opcode),
                        try self.nextBytes(16),
                    );
                },

                0...11, 92, 93 => |opcode| {
                    try visitor.visitVectorMemoryInstruction(
                        @intToEnum(VectorMemoryInstruction, opcode),
                        try self.nextMemoryArgument(),
                    );
                },

                21...34 => |opcode| {
                    try visitor.visitVectorLaneInstruction(
                        @intToEnum(VectorLaneInstruction, opcode),
                        try self.nextByte(),
                    );
                },

                84...91 => |opcode| {
                    try visitor.visitVectorMemoryLaneInstruction(
                        @intToEnum(VectorMemoryLaneInstruction, opcode),
                        try self.nextMemoryArgument(),
                        try self.nextByte(),
                    );
                },

                else => return error.UnsupportedInstruction,
            },

            else => return error.UnsupportedInstruction,
        }
    }
};

pub const ModuleDirectory = struct {
    types: std.ArrayListUnmanaged(FunctionType) = .{},
    imported_functions: std.ArrayListUnmanaged(ImportedFunction) = .{},
    imported_tables: std.ArrayListUnmanaged(ImportedTable) = .{},
    imported_memories: std.ArrayListUnmanaged(ImportedMemory) = .{},
    imported_globals: std.ArrayListUnmanaged(ImportedGlobal) = .{},
    functions: std.ArrayListUnmanaged(TypeIndex) = .{},
    tables: std.ArrayListUnmanaged(TableType) = .{},
    memories: std.ArrayListUnmanaged(MemoryType) = .{},
    globals: std.ArrayListUnmanaged(Global) = .{},
    exports: std.ArrayListUnmanaged(Export) = .{},
    start: ?FunctionIndex = null,
    element_segments: std.ArrayListUnmanaged(ElementSegment) = .{},
    code: std.ArrayListUnmanaged([]const u8) = .{},
    data_segments: std.ArrayListUnmanaged(DataSegment) = .{},
    module_name: ?[]const u8 = null,
    function_names: std.AutoHashMapUnmanaged(FunctionIndex, []const u8) = .{},
    local_names: std.AutoHashMapUnmanaged(struct { FunctionIndex, LocalIndex }, []const u8) = .{},

    pub fn init(allocator: std.mem.Allocator, module_data: []const u8) !@This() {
        var directory = @This(){};
        try directory.processModule(allocator, module_data);
        return directory;
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        for (self.element_segments.items) |*segment| {
            switch (segment.contents) {
                inline else => |contents| allocator.free(contents),
            }
        }

        self.types.deinit(allocator);
        self.imported_functions.deinit(allocator);
        self.imported_tables.deinit(allocator);
        self.imported_memories.deinit(allocator);
        self.imported_globals.deinit(allocator);
        self.functions.deinit(allocator);
        self.tables.deinit(allocator);
        self.memories.deinit(allocator);
        self.globals.deinit(allocator);
        self.exports.deinit(allocator);
        self.element_segments.deinit(allocator);
        self.code.deinit(allocator);
        self.data_segments.deinit(allocator);
        self.function_names.deinit(allocator);
        self.local_names.deinit(allocator);
    }

    fn processModule(self: *@This(), allocator: std.mem.Allocator, data: []const u8) !void {
        var decoder = Decoder.init(data);
        while (!decoder.atEnd()) {
            const section_id = try decoder.nextByte();
            const section_len = try decoder.nextInt(u32);
            const section_data = try decoder.nextBytes(section_len);
            try self.processSection(allocator, section_id, section_data);
        }
    }

    fn processSection(self: *@This(), allocator: std.mem.Allocator, id: u8, data: []const u8) !void {
        var decoder = Decoder.init(data);
        switch (id) {
            0 => {
                const name = try decoder.nextName();
                if (std.mem.eql(u8, name, "name")) {
                    self.processNameSection(allocator, decoder.remainder()) catch |err| {
                        std.log.err("error parsing 'name' section: {}", .{err});
                        self.module_name = null;
                        self.function_names.clearAndFree(allocator);
                        self.local_names.clearAndFree(allocator);
                    };
                }
            },

            1 => {
                const len = try decoder.nextInt(u32);
                try self.types.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.types.appendAssumeCapacity(try decoder.nextFunctionType());
            },

            2 => {
                const len = try decoder.nextInt(u32);
                for (0..len) |_| {
                    const name = ImportName{
                        .module = try decoder.nextName(),
                        .name = try decoder.nextName(),
                    };

                    switch (try decoder.nextByte()) {
                        0x00 => try self.imported_functions.append(allocator, .{
                            .name = name,
                            .type = try self.nextIndex(&decoder, TypeIndex),
                        }),

                        0x01 => try self.imported_tables.append(allocator, .{
                            .name = name,
                            .type = try decoder.nextTableType(),
                        }),

                        0x02 => try self.imported_memories.append(allocator, .{
                            .name = name,
                            .type = try decoder.nextMemoryType(),
                        }),

                        0x03 => try self.imported_globals.append(allocator, .{
                            .name = name,
                            .type = try decoder.nextGlobalType(),
                        }),

                        else => return error.UnsupportedImport,
                    }
                }
            },

            3 => {
                const len = try decoder.nextInt(u32);
                try self.functions.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.functions.appendAssumeCapacity(try self.nextIndex(&decoder, TypeIndex));
            },

            4 => {
                const len = try decoder.nextInt(u32);
                try self.tables.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.tables.appendAssumeCapacity(try decoder.nextTableType());
            },

            5 => {
                const len = try decoder.nextInt(u32);
                try self.memories.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.memories.appendAssumeCapacity(try decoder.nextMemoryType());
            },

            6 => {
                const len = try decoder.nextInt(u32);
                try self.globals.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.globals.appendAssumeCapacity(try self.nextGlobal(&decoder));
            },

            7 => {
                const len = try decoder.nextInt(u32);
                try self.exports.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.exports.appendAssumeCapacity(try self.nextExport(&decoder));
            },

            8 => {
                self.start = try self.nextIndex(&decoder, FunctionIndex);
            },

            9 => {
                const len = try decoder.nextInt(u32);
                try self.element_segments.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| self.element_segments.appendAssumeCapacity(try self.nextElementSegment(allocator, &decoder));
            },

            else => return error.UnsupportedSection,
        }
    }

    fn processNameSection(self: *@This(), allocator: std.mem.Allocator, data: []const u8) !void {
        var decoder = Decoder.init(data);
        while (!decoder.atEnd()) {
            const subsection_id = try decoder.nextByte();
            const subsection_len = try decoder.nextInt(u32);
            const subsection_data = try decoder.nextBytes(subsection_len);
            try self.processNameSubsection(allocator, subsection_id, subsection_data);
        }
    }

    fn processNameSubsection(self: *@This(), allocator: std.mem.Allocator, id: u8, data: []const u8) !void {
        var decoder = Decoder.init(data);
        switch (id) {
            0 => {
                self.module_name = try decoder.nextName();
            },

            1 => {
                const len = try decoder.nextInt(u32);
                try self.function_names.ensureUnusedCapacity(allocator, len);
                for (0..len) |_| {
                    const function = try self.nextIndex(&decoder, FunctionIndex);
                    const function_name = try decoder.nextName();
                    self.function_names.putAssumeCapacity(function, function_name);
                }
            },

            2 => {
                const outer_len = try decoder.nextInt(u32);
                for (0..outer_len) |_| {
                    const function = try self.nextIndex(&decoder, FunctionIndex);
                    const inner_len = try decoder.nextInt(u32);
                    try self.local_names.ensureUnusedCapacity(allocator, inner_len);
                    for (0..inner_len) |_| {
                        const local = LocalIndex{ .value = try decoder.nextInt(u32) };
                        const name = try decoder.nextName();
                        self.local_names.putAssumeCapacity(.{ function, local }, name);
                    }
                }
            },

            else => {},
        }
    }

    fn validateIndex(self: *@This(), idx: anytype) !@TypeOf(idx) {
        switch (@TypeOf(idx)) {
            TypeIndex => if (idx.value < self.types.items.len)
                return error.TypeIndexOutOfBounds,

            FunctionIndex => if (idx.value >= self.imported_functions.items.len and idx.value - self.imported_functions.items.len >= self.functions.items.len)
                return error.FunctionIndexOutOfBounds,

            TableIndex => if (idx.value >= self.imported_tables.items.len and idx.value - self.imported_tables.items.len >= self.tables.items.len)
                return error.TableIndexOutOfBounds,

            MemoryIndex => if (idx.value >= self.imported_memories.items.len and idx.value - self.imported_memories.items.len >= self.memories.items.len)
                return error.MemoryIndexOutOfBounds,

            GlobalIndex => if (idx.value >= self.imported_globals.items.len and idx.value - self.imported_globals.items.len >= self.globals.items.len)
                return error.GlobalIndexOutOfBounds,

            else => unreachable,
        }

        return idx;
    }

    fn nextIndex(self: *@This(), decoder: *Decoder, comptime T: type) !T {
        return try self.validateIndex(T{ .value = try decoder.nextInt(u32) });
    }

    fn isImport(self: *@This(), idx: anytype) bool {
        const imports_len = switch (@TypeOf(idx)) {
            FunctionIndex => self.imported_functions.items.len,
            TableIndex => self.imported_tables.items.len,
            MemoryIndex => self.imported_memories.items.len,
            GlobalIndex => self.imported_globals.items.len,
            else => unreachable,
        };

        return idx.value < imports_len;
    }

    fn nextConstantGlobalGet(self: *@This(), decoder: *Decoder, expected_type: ValueType) !GlobalIndex {
        const idx = try self.nextIndex(decoder, GlobalIndex);

        if (!self.isImport(idx))
            return error.UnsupportedConstantExpression;

        if (self.imported_globals.items[idx.value].type.value_type != expected_type)
            return error.TypeMismatch;

        return idx;
    }

    fn nextConstantExpression(self: *@This(), decoder: *Decoder, expected_type: ValueType) !ConstantExpression {
        const value: ConstantExpression = switch (try decoder.nextByte()) {
            opcodes.@"global.get" => .{ .global_get = try self.nextConstantGlobalGet(decoder, expected_type) },

            opcodes.@"i32.const" => if (expected_type == .i32)
                .{ .i32_const = try decoder.nextInt(i32) }
            else
                return error.TypeMismatch,

            opcodes.@"i64.const" => if (expected_type == .i64)
                .{ .i64_const = try decoder.nextInt(i64) }
            else
                return error.TypeMismatch,

            opcodes.@"f32.const" => if (expected_type == .f32)
                .{ .f32_const = try decoder.nextFixedWidth(u32) }
            else
                return error.TypeMismatch,

            opcodes.@"f64.const" => if (expected_type == .f64)
                .{ .f64_const = try decoder.nextFixedWidth(u64) }
            else
                return error.TypeMismatch,

            opcodes.@"ref.null" => if (expected_type == .funcref or expected_type == .externref)
                .ref_null
            else
                return error.TypeMismatch,

            opcodes.@"ref.func" => if (expected_type == .funcref)
                .{ .ref_func = try self.nextIndex(decoder, FunctionIndex) }
            else
                return error.TypeMismatch,

            else => return error.UnsupportedConstantExpression,
        };

        if (try decoder.nextByte() != opcodes.end)
            return error.UnsupportedConstantExpression;

        return value;
    }

    fn nextGlobal(self: *@This(), decoder: *Decoder) !Global {
        const typ = try decoder.nextGlobalType();
        return .{
            .type = typ,
            .initial_value = try self.nextConstantExpression(decoder, typ.value_type),
        };
    }

    fn nextExport(self: *@This(), decoder: *Decoder) !Export {
        return .{
            .name = try decoder.nextName(),

            .desc = switch (try decoder.nextByte()) {
                0x00 => .{ .function = try self.nextIndex(decoder, FunctionIndex) },
                0x01 => .{ .table = try self.nextIndex(decoder, TableIndex) },
                0x02 => .{ .memory = try self.nextIndex(decoder, MemoryIndex) },
                0x03 => .{ .global = try self.nextIndex(decoder, GlobalIndex) },
                else => return error.UnsupportedExport,
            },
        };
    }

    fn nextI32ConstantExpression(self: *@This(), decoder: *Decoder) !I32ConstantExpression {
        const value: I32ConstantExpression = switch (try decoder.nextByte()) {
            opcodes.@"global.get" => .{ .global_get = try self.nextConstantGlobalGet(decoder, .i32) },
            opcodes.@"i32.const" => .{ .i32_const = try decoder.nextInt(i32) },
            else => return error.UnsupportedI32ConstantExpression,
        };

        if (try decoder.nextByte() != opcodes.end)
            return error.UnsupportedI32ConstantExpression;

        return value;
    }

    fn nextFuncrefConstantExpression(self: *@This(), decoder: *Decoder) !FuncrefConstantExpression {
        const value: FuncrefConstantExpression = switch (try decoder.nextByte()) {
            opcodes.@"global.get" => .{ .global_get = try self.nextConstantGlobalGet(decoder, .funcref) },
            opcodes.@"ref.null" => .ref_null,
            opcodes.@"ref.func" => .{ .ref_func = try self.nextIndex(decoder, FunctionIndex) },
            else => return error.UnsupportedFuncrefConstantExpression,
        };

        if (try decoder.nextByte() != opcodes.end)
            return error.UnsupportedFuncrefConstantExpression;

        return value;
    }

    fn nextExternrefConstantExpression(self: *@This(), decoder: *Decoder) !ExternrefConstantExpression {
        const value: ExternrefConstantExpression = switch (try decoder.nextByte()) {
            opcodes.@"global.get" => .{ .global_get = try self.nextConstantGlobalGet(decoder, .externref) },
            opcodes.@"ref.null" => .ref_null,
            else => return error.UnsupportedExternrefConstantExpression,
        };

        if (try decoder.nextByte() != opcodes.end)
            return error.UnsupportedExternrefConstantExpression;

        return value;
    }

    fn nextIndexVector(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder, comptime T: type) ![]const T {
        const len = try decoder.nextInt(u32);
        const indices = try allocator.alloc(T, len);
        errdefer allocator.free(indices);
        for (indices) |*idx| idx.* = try self.nextIndex(decoder, T);
        return indices;
    }

    fn nextFuncrefConstantExpressionVector(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) ![]const FuncrefConstantExpression {
        const len = try decoder.nextInt(u32);
        const exprs = try allocator.alloc(FuncrefConstantExpression, len);
        errdefer allocator.free(exprs);
        for (exprs) |*expr| expr.* = try self.nextFuncrefConstantExpression(decoder);
        return exprs;
    }

    fn nextExternrefConstantExpressionVector(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) ![]const ExternrefConstantExpression {
        const len = try decoder.nextInt(u32);
        const exprs = try allocator.alloc(ExternrefConstantExpression, len);
        errdefer allocator.free(exprs);
        for (exprs) |*expr| expr.* = try self.nextExternrefConstantExpression(decoder);
        return exprs;
    }

    fn nextKindedElementSegmentInit(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) !ElementSegment.Init {
        return switch (try decoder.nextElementSegmentKind()) {
            .funcref => .{ .funcrefs = try self.nextIndexVector(allocator, decoder, FunctionIndex) },
        };
    }

    fn nextTypedElementSegmentInit(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) !ElementSegment.Init {
        return switch (try decoder.nextReferenceType()) {
            .funcref => .{ .funcref_exprs = try self.nextFuncrefConstantExpressionVector(allocator, decoder) },
            .externref => .{ .externref_exprs = try self.nextExternrefConstantExpressionVector(allocator, decoder) },
        };
    }

    fn nextElementSegment(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) !ElementSegment {
        return switch (try decoder.nextInt(u32)) {
            0 => .{
                .mode = .{ .active = .{
                    .table = try self.validateIndex(TableIndex{ .value = 0 }),
                    .offset = try self.nextI32ConstantExpression(decoder),
                } },

                .init = .{ .funcrefs = try self.nextIndexVector(allocator, decoder, FunctionIndex) },
            },

            1 => .{
                .mode = .passive,
                .init = try self.nextKindedElementSegmentInit(allocator, decoder),
            },

            2 => .{
                .mode = .{ .active = .{
                    .table = try self.nextIndex(decoder, TableIndex),
                    .offset = try self.nextI32ConstantExpression(decoder),
                } },

                .init = .{ .funcrefs = try self.nextKindedElementSegmentInit(allocator, decoder) },
            },

            3 => .{
                .mode = .declarative,
                .init = .{ .funcrefs = try self.nextKindedElementSegmentInit(allocator, decoder) },
            },

            4 => .{
                .mode = .{ .active = .{
                    .table = try self.validateIndex(TableIndex{ .value = 0 }),
                    .offset = try self.nextI32ConstantExpression(decoder),
                } },

                .init = .{ .funcref_exprs = try self.nextFuncrefConstantExpressionVector(allocator, decoder) },
            },

            5 => .{
                .mode = .passive,
                .init = try self.nextTypedElementSegmentInit(allocator, decoder),
            },

            6 => .{
                .mode = .{ .active = .{
                    .table = try self.nextIndex(decoder, TableIndex),
                    .offset = try self.nextI32ConstantExpression(decoder),
                } },

                .init = try self.nextTypedElementSegmentInit(allocator, decoder),
            },

            7 => .{
                .mode = .declarative,
                .init = try self.nextTypedElementSegmentInit(allocator, decoder),
            },

            else => error.UnsupportedElementSegment,
        };
    }
};

pub const opcodes = struct {
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
};

test "ref all" {
    std.testing.refAllDeclsRecursive(@This());
}
