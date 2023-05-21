const std = @import("std");

pub const TypeIdx = u32;
pub const FuncIdx = u32;
pub const TableIdx = u32;
pub const MemIdx = u32;
pub const GlobalIdx = u32;
pub const ElemIdx = u32;
pub const DataIdx = u32;
pub const LocalIdx = u32;
pub const LabelIdx = u32;
pub const LaneIdx = u8;

pub const SingleByteInstr = enum(u8) {
    @"unreachable" = 0x00,
    nop,

    @"else" = 0x05,

    end = 0x0b,

    @"return" = 0x0f,

    drop = 0x1a,
    select,

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

pub const BlockTypeInstr = enum(u8) {
    block = 0x02,
    loop,
    @"if",
};

pub const IdxInstr = enum(u8) {
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

pub const MemargInstr = enum(u8) {
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

pub const ExtendedInstr = enum(u32) {
    i32_trunc_sat_f32_s = 0,
    i32_trunc_sat_f32_u,
    i32_trunc_sat_f64_s,
    i32_trunc_sat_f64_u,
    i64_trunc_sat_f32_s,
    i64_trunc_sat_f32_u,
    i64_trunc_sat_f64_s,
    i64_trunc_sat_f64_u,
};

pub const ExtendedIdxInstr = enum(u32) {
    data_drop = 9,

    memory_fill = 11,

    elem_drop = 13,

    table_grow = 15,
    table_size,
    table_fill,
};

pub const ExtendedDualIdxInstr = enum(u32) {
    memory_init = 8,
    memory_copy = 10,
    table_init = 12,
    table_copy = 14,
};

pub const VectorInstr = enum(u32) {
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

pub const VectorMemargInstr = enum(u32) {
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

pub const VectorLaneidxInstr = enum(u32) {
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

pub const VectorMemargLaneidxInstr = enum(u32) {
    v128_load8_lane = 84,
    v128_load16_lane,
    v128_load32_lane,
    v128_load64_lane,
    v128_store8_lane,
    v128_store16_lane,
    v128_store32_lane,
    v128_store64_lane,
};

pub const Instr = union(enum) {
    single_byte: SingleByteInstr,
    block_type: struct { BlockTypeInstr, BlockType },
    idx: struct { IdxInstr, u32 },
    memarg: struct { MemargInstr, MemArg },
    extended: ExtendedInstr,
    extended_idx: struct { ExtendedIdxInstr, u32 },
    extended_dual_idx: struct { ExtendedDualIdxInstr, u32, u32 },
    vector: VectorInstr,
    vector_memarg: struct { VectorMemargInstr, MemArg },
    vector_laneidx: struct { VectorLaneidxInstr, LaneIdx },
    vector_memarg_laneidx: struct { VectorMemargLaneidxInstr, MemArg, LaneIdx },

    br: LabelIdx,
    br_if: LabelIdx,
    br_table: void,
    call: FuncIdx,
    call_indirect: struct { TypeIdx, TableIdx },
    ref_null: RefType,
    ref_is_null: void,
    ref_func: FuncIdx,
    drop: void,
    select: void,
    select_t: void,
    local_get: LocalIdx,
    local_set: LocalIdx,
    local_tee: LocalIdx,
    global_get: GlobalIdx,
    global_set: GlobalIdx,
    table_get: TableIdx,
    table_set: TableIdx,
    table_init: struct { ElemIdx, TableIdx },
    elem_drop: ElemIdx,
    table_copy: struct { TableIdx, TableIdx },
    table_grow: TableIdx,
    table_size: TableIdx,
    table_fill: TableIdx,
    memory_size: void,
    memory_grow: void,
    memory_init: DataIdx,
    data_drop: DataIdx,
    memory_copy: void,
    memory_fill: void,
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,

    v128_const: i128,
    i8x16_shuffle: [16]LaneIdx,
};

pub const NumType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
};

pub const VecType = enum(u8) {
    v128 = 0x7b,
};

pub const RefType = enum(u8) {
    funcref = 0x70,
    externref = 0x6f,
};

pub const ValType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
    v128 = 0x7b,
    funcref = 0x70,
    externref = 0x6f,
};

pub const ResultType = []const ValType;

pub const FuncType = struct {
    parameters: ResultType,
    results: ResultType,
};

pub const Limits = struct {
    min: u32,
    max: ?u32 = null,
};

pub const MemType = struct {
    limits: Limits,
};

pub const TableType = struct {
    element_type: RefType,
    limits: Limits,
};

pub const Mut = enum(u8) {
    @"const" = 0x00,
    @"var" = 0x01,
};

pub const GlobalType = struct {
    value_type: ValType,
    mut: Mut,
};

pub const MemArg = struct {
    alignment: u32,
    offset: u32,
};

pub const Name = []const u8;

pub const ImportDesc = union(enum) {
    func: FuncIdx,
    table: TableType,
    mem: MemType,
    global: GlobalType,
};

pub const Import = struct {
    module_name: []const u8,
    name: []const u8,
    desc: ImportDesc,
};

pub const ConstantExpr = union(enum) {
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,
    ref_null: void,
    ref_func: FuncIdx,
    global_get: GlobalIdx,
};

pub const BlockType = union(enum) {
    immediate: ?ValType,
    indexed: FuncIdx,
};

pub const Global = struct {
    type: GlobalType,
    initial_value: ConstantExpr,
};

pub const ExportDesc = union(enum) {
    func: FuncIdx,
    table: TableIdx,
    mem: MemIdx,
    global: GlobalIdx,
};

pub const Export = struct {
    name: Name,
    desc: ExportDesc,
};

pub const ElemKind = enum(u8) {
    funcref = 0x00,
};

pub const SectionId = enum(u8) {
    custom,
    type,
    import,
    function,
    table,
    memory,
    global,
    @"export",
    start,
    element,
    code,
    data,
    data_count,
};

pub const Section = struct {
    id: SectionId,
    data: []const u8,
};

pub const SectionIterator = struct {
    decoder: Decoder,

    pub fn init(module_data: []const u8) !@This() {
        var decoder = Decoder.init(module_data);

        const magic = try decoder.nextBytes(4);
        if (!std.meta.eql(magic.*, [_]u8{ 0x00, 0x61, 0x73, 0x6d }))
            return error.NotABinaryWasmModule;

        const version = try decoder.nextBytes(4);
        if (!std.meta.eql(version.*, [_]u8{ 0x01, 0x00, 0x00, 0x00 }))
            return error.UnsupportedBinaryFormatVersion;

        return .{ .decoder = decoder };
    }

    pub fn next(self: *@This()) !?Section {
        if (self.decoder.atEnd()) return null;
        const id = try self.decoder.nextSectionId();
        const size = try self.decoder.nextInt(u32);
        const data = try self.decoder.nextBytes(size);
        return Section{ .id = id, .data = data };
    }
};

pub const Decoder = struct {
    data: []const u8,

    pub fn init(data: []const u8) @This() {
        return .{ .data = data };
    }

    pub fn hasRemainingData(self: @This()) bool {
        return self.data.len != 0;
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

    pub fn nextFloat(self: *@This(), comptime T: type) !T {
        const Bits = std.meta.Int(.unsigned, @typeInfo(T).Float.bits);
        return @bitCast(T, std.mem.readIntLittle(Bits, try self.nextBytes(@sizeOf(T))));
    }

    pub fn nextName(self: *@This()) !Name {
        return try self.nextBytes(try self.nextInt(u32));
    }

    pub fn nextNumType(self: *@This()) !NumType {
        return std.meta.intToEnum(NumType, try self.nextByte()) catch
            error.UnsupportedNumberType;
    }

    pub fn nextVecType(self: *@This()) !VecType {
        return std.meta.intToEnum(VecType, try self.nextByte()) catch
            error.UnsupportedVectorType;
    }

    pub fn nextRefType(self: *@This()) !RefType {
        return std.meta.intToEnum(RefType, try self.nextByte()) catch
            error.UnsupportedReferenceType;
    }

    pub fn nextValType(self: *@This()) !ValType {
        return std.meta.intToEnum(ValType, try self.nextByte()) catch
            error.UnsupportedValueType;
    }

    pub fn nextResultType(self: *@This()) !ResultType {
        const len = try self.nextInt(u32);
        const types = try self.nextBytes(len);

        for (types) |t|
            _ = std.meta.intToEnum(ValType, t) catch
                return error.UnsupportedValueType;

        return std.mem.bytesAsSlice(ValType, types);
    }

    pub fn nextFuncType(self: *@This()) !FuncType {
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

    pub fn nextMemType(self: *@This()) !MemType {
        return .{ .limits = try self.nextLimits() };
    }

    pub fn nextTableType(self: *@This()) !TableType {
        return .{
            .element_type = try self.nextRefType(),
            .limits = try self.nextLimits(),
        };
    }

    pub fn nextMut(self: *@This()) !Mut {
        return std.meta.intToEnum(Mut, try self.nextByte()) catch
            error.UnsupportedMutability;
    }

    pub fn nextElemKind(self: *@This()) !ElemKind {
        return std.meta.intToEnum(ElemKind, try self.nextByte()) catch
            error.UnsupportedElemKind;
    }

    pub fn nextGlobalType(self: *@This()) !GlobalType {
        return .{
            .value_type = try self.nextValType(),
            .mut = try self.nextMut(),
        };
    }

    pub fn nextBlockType(self: *@This()) !BlockType {
        if (self.data.len == 0)
            return error.UnexpectedEndOfData;

        return switch (self.data[0]) {
            0x40 => blk: {
                self.data = self.data[1..];
                break :blk .{ .immediate = null };
            },

            0x6f, 0x70, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f => |value_type| blk: {
                self.data = self.data[1..];
                break :blk .{ .immediate = @intToEnum(ValType, value_type) };
            },

            else => blk: {
                const index = try self.nextInt(i33);
                if (index < 0) return error.UnsupportedBlockType;
                break :blk .{ .indexed = @intCast(FuncIdx, index) };
            },
        };
    }

    pub fn nextMemArg(self: *@This()) !MemArg {
        return .{
            .alignment = try self.nextInt(u32),
            .offset = try self.nextInt(u32),
        };
    }

    pub fn nextSectionId(self: *@This()) !SectionId {
        return std.meta.intToEnum(SectionId, try self.nextByte()) catch
            error.UnsupportedSectionId;
    }

    pub fn nextImportDesc(self: *@This()) !ImportDesc {
        return switch (try self.nextByte()) {
            0x00 => .{ .func = try self.nextInt(FuncIdx) },
            0x01 => .{ .table = try self.nextTableType() },
            0x02 => .{ .mem = try self.nextMemType() },
            0x03 => .{ .global = try self.nextGlobalType() },
            else => error.UnsupportedImportDesc,
        };
    }

    pub fn nextImport(self: *@This()) !Import {
        return .{
            .module_name = try self.nextName(),
            .name = try self.nextName(),
            .desc = try self.nextImportDesc(),
        };
    }

    pub fn nextConstantExpr(self: *@This()) !ConstantExpr {
        const value: ConstantExpr = switch (try self.nextByte()) {
            0x23 => .{ .global_get = try self.nextInt(GlobalIdx) },
            0x41 => .{ .i32_const = try self.nextInt(i32) },
            0x42 => .{ .i64_const = try self.nextInt(i64) },
            0x43 => .{ .f32_const = try self.nextFloat(f32) },
            0x44 => .{ .f64_const = try self.nextFloat(f64) },
            0xd0 => .ref_null,
            0xd2 => .{ .ref_func = try self.nextInt(FuncIdx) },
            else => return error.UnsupportedConstantExpression,
        };

        if (try self.nextByte() != 0x0b)
            return error.UnsupportedConstantExpression;

        return value;
    }

    pub fn nextGlobal(self: *@This()) !Global {
        return .{
            .type = try self.nextGlobalType(),
            .initial_value = try self.nextConstantExpr(),
        };
    }

    pub fn nextExportDesc(self: *@This()) !ExportDesc {
        return switch (try self.nextByte()) {
            0x00 => .{ .func = try self.nextInt(FuncIdx) },
            0x01 => .{ .table = try self.nextInt(TableIdx) },
            0x02 => .{ .mem = try self.nextInt(MemIdx) },
            0x03 => .{ .global = try self.nextInt(GlobalIdx) },
            else => error.UnsupportedExportDesc,
        };
    }

    pub fn nextExport(self: *@This()) !Export {
        return .{
            .name = try self.nextName(),
            .desc = try self.nextExportDesc(),
        };
    }

    pub fn nextInstr(self: *@This()) !Instr {
        return switch (try self.nextByte()) {
            @enumToInt(SingleByteInstr.@"unreachable")...@enumToInt(SingleByteInstr.nop),
            @enumToInt(SingleByteInstr.@"else"),
            @enumToInt(SingleByteInstr.end),
            @enumToInt(SingleByteInstr.@"return"),
            @enumToInt(SingleByteInstr.drop)...@enumToInt(SingleByteInstr.select),
            @enumToInt(SingleByteInstr.i32_eqz)...@enumToInt(SingleByteInstr.i64_extend32_s),
            @enumToInt(SingleByteInstr.ref_is_null),
            => |opcode| .{ .single_byte = @intToEnum(SingleByteInstr, opcode) },

            @enumToInt(BlockTypeInstr.block)...@enumToInt(BlockTypeInstr.@"if") => |opcode| .{
                .block_type = .{
                    @intToEnum(BlockTypeInstr, opcode),
                    try self.nextBlockType(),
                },
            },

            @enumToInt(IdxInstr.br)...@enumToInt(IdxInstr.br_if),
            @enumToInt(IdxInstr.call),
            @enumToInt(IdxInstr.local_get)...@enumToInt(IdxInstr.table_set),
            @enumToInt(IdxInstr.memory_size)...@enumToInt(IdxInstr.memory_grow),
            @enumToInt(IdxInstr.ref_func),
            => |opcode| .{ .idx = .{
                @intToEnum(IdxInstr, opcode),
                try self.nextInt(u32),
            } },

            @enumToInt(MemargInstr.i32_load)...@enumToInt(MemargInstr.i64_store32) => |opcode| .{
                .memarg = .{
                    @intToEnum(MemargInstr, opcode),
                    try self.nextMemArg(),
                },
            },

            0xfc => switch (try self.nextInt(u32)) {
                @enumToInt(ExtendedInstr.i32_trunc_sat_f32_s)...@enumToInt(ExtendedInstr.i64_trunc_sat_f64_u) => |opcode| .{
                    .extended = @intToEnum(ExtendedInstr, opcode),
                },

                @enumToInt(ExtendedIdxInstr.data_drop),
                @enumToInt(ExtendedIdxInstr.memory_fill),
                @enumToInt(ExtendedIdxInstr.elem_drop),
                @enumToInt(ExtendedIdxInstr.table_grow)...@enumToInt(ExtendedIdxInstr.table_fill),
                => |opcode| .{ .extended_idx = .{
                    @intToEnum(ExtendedIdxInstr, opcode),
                    try self.nextInt(u32),
                } },

                @enumToInt(ExtendedDualIdxInstr.memory_init),
                @enumToInt(ExtendedDualIdxInstr.memory_copy),
                @enumToInt(ExtendedDualIdxInstr.table_init),
                @enumToInt(ExtendedDualIdxInstr.table_copy),
                => |opcode| .{ .extended_dual_idx = .{
                    @intToEnum(ExtendedDualIdxInstr, opcode),
                    try self.nextInt(u32),
                    try self.nextInt(u32),
                } },

                else => error.UnsupportedInstruction,
            },

            0xfd => switch (try self.nextInt(u32)) {
                @enumToInt(VectorInstr.i8x16_swizzle)...@enumToInt(VectorInstr.f64x2_splat),
                @enumToInt(VectorInstr.i8x16_eq)...@enumToInt(VectorInstr.v128_any_true),
                @enumToInt(VectorInstr.f32x4_demote_f64x2_zero)...@enumToInt(VectorInstr.i16x8_max_u),
                @enumToInt(VectorInstr.i16x8_avgr_u)...@enumToInt(VectorInstr.i32x4_neg),
                @enumToInt(VectorInstr.i32x4_all_true)...@enumToInt(VectorInstr.i32x4_bitmask),
                @enumToInt(VectorInstr.i32x4_extend_low_i16x8_s)...@enumToInt(VectorInstr.i32x4_add),
                @enumToInt(VectorInstr.i32x4_sub),
                @enumToInt(VectorInstr.i32x4_mul)...@enumToInt(VectorInstr.i32x4_dot_i16x8_s),
                @enumToInt(VectorInstr.i32x4_extmul_low_i16x8_s)...@enumToInt(VectorInstr.i64x2_neg),
                @enumToInt(VectorInstr.i64x2_all_true)...@enumToInt(VectorInstr.i64x2_bitmask),
                @enumToInt(VectorInstr.i64x2_extend_low_i32x4_s)...@enumToInt(VectorInstr.i64x2_add),
                @enumToInt(VectorInstr.i64x2_sub),
                @enumToInt(VectorInstr.i64x2_mul)...@enumToInt(VectorInstr.f64x2_neg),
                @enumToInt(VectorInstr.f64x2_sqrt)...@enumToInt(VectorInstr.f64x2_convert_low_i32x4_u),
                => |opcode| .{ .vector = @intToEnum(VectorInstr, opcode) },

                @enumToInt(VectorMemargInstr.v128_load)...@enumToInt(VectorMemargInstr.v128_store),
                @enumToInt(VectorMemargInstr.v128_load32_zero)...@enumToInt(VectorMemargInstr.v128_load64_zero),
                => |opcode| .{ .vector_memarg = .{
                    @intToEnum(VectorMemargInstr, opcode),
                    try self.nextMemArg(),
                } },

                @enumToInt(VectorLaneidxInstr.i8x16_extract_lane_s)...@enumToInt(VectorLaneidxInstr.f64x2_replace_lane) => |opcode| .{
                    .vector_laneidx = .{
                        @intToEnum(VectorLaneidxInstr, opcode),
                        try self.nextByte(),
                    },
                },

                @enumToInt(VectorMemargLaneidxInstr.v128_load8_lane)...@enumToInt(VectorMemargLaneidxInstr.v128_store64_lane) => |opcode| .{
                    .vector_memarg_laneidx = .{
                        @intToEnum(VectorMemargLaneidxInstr, opcode),
                        try self.nextMemArg(),
                        try self.nextByte(),
                    },
                },

                else => error.UnsupportedInstruction,
            },

            else => error.UnsupportedInstruction,
        };
    }
};

test "ref all" {
    std.testing.refAllDeclsRecursive(@This());
}
