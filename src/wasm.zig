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

pub const Instr = union(enum) {
    @"unreachable": void,
    nop: void,
    block: void,
    loop: void,
    @"if": void,
    @"else": void,
    end: void,
    br: LabelIdx,
    br_if: LabelIdx,
    br_table: void,
    @"return": void,
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
    i32_load: MemArg,
    i64_load: MemArg,
    f32_load: MemArg,
    f64_load: MemArg,
    i32_load8_s: MemArg,
    i32_load8_u: MemArg,
    i32_load16_s: MemArg,
    i32_load16_u: MemArg,
    i64_load8_s: MemArg,
    i64_load8_u: MemArg,
    i64_load16_s: MemArg,
    i64_load16_u: MemArg,
    i64_load32_s: MemArg,
    i64_load32_u: MemArg,
    i32_store: MemArg,
    i64_store: MemArg,
    f32_store: MemArg,
    f64_store: MemArg,
    i32_store8: MemArg,
    i32_store16: MemArg,
    i64_store8: MemArg,
    i64_store16: MemArg,
    i64_store32: MemArg,
    memory_size: void,
    memory_init: DataIdx,
    data_drop: DataIdx,
    memory_copy: void,
    memory_fill: void,
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,
    i32_eqz: void,
    i32_eq: void,
    i32_ne: void,
    i32_lt_s: void,
    i32_lt_u: void,
    i32_gt_s: void,
    i32_gt_u: void,
    i32_le_s: void,
    i32_le_u: void,
    i32_ge_s: void,
    i32_ge_u: void,
    i64_eqz: void,
    i64_eq: void,
    i64_ne: void,
    i64_lt_s: void,
    i64_lt_u: void,
    i64_gt_s: void,
    i64_gt_u: void,
    i64_le_s: void,
    i64_le_u: void,
    i64_ge_s: void,
    i64_ge_u: void,
    f32_eq: void,
    f32_ne: void,
    f32_lt: void,
    f32_gt: void,
    f32_le: void,
    f32_ge: void,
    f64_eq: void,
    f64_ne: void,
    f64_lt: void,
    f64_gt: void,
    f64_le: void,
    f64_ge: void,
    i32_clz: void,
    i32_ctz: void,
    i32_popcnt: void,
    i32_add: void,
    i32_sub: void,
    i32_mul: void,
    i32_div_s: void,
    i32_div_u: void,
    i32_rem_s: void,
    i32_rem_u: void,
    i32_and: void,
    i32_or: void,
    i32_xor: void,
    i32_shl: void,
    i32_shr_s: void,
    i32_shr_u: void,
    i32_rotl: void,
    i32_rotr: void,
    i64_clz: void,
    i64_ctz: void,
    i64_popcnt: void,
    i64_add: void,
    i64_sub: void,
    i64_mul: void,
    i64_div_s: void,
    i64_div_u: void,
    i64_rem_s: void,
    i64_rem_u: void,
    i64_and: void,
    i64_or: void,
    i64_xor: void,
    i64_shl: void,
    i64_shr_s: void,
    i64_shr_u: void,
    i64_rotl: void,
    i64_rotr: void,
    f32_abs: void,
    f32_neg: void,
    f32_ceil: void,
    f32_floor: void,
    f32_trunc: void,
    f32_nearest: void,
    f32_sqrt: void,
    f32_add: void,
    f32_sub: void,
    f32_mul: void,
    f32_div: void,
    f32_min: void,
    f32_max: void,
    f32_copysign: void,
    f64_abs: void,
    f64_neg: void,
    f64_ceil: void,
    f64_floor: void,
    f64_trunc: void,
    f64_nearest: void,
    f64_sqrt: void,
    f64_add: void,
    f64_sub: void,
    f64_mul: void,
    f64_div: void,
    f64_min: void,
    f64_max: void,
    f64_copysign: void,
    i32_wrap_i64: void,
    i32_trunc_f32_s: void,
    i32_trunc_f32_u: void,
    i32_trunc_f64_s: void,
    i32_trunc_f64_u: void,
    i64_extend_i32_s: void,
    i64_extend_i32_u: void,
    i64_trunc_f32_s: void,
    i64_trunc_f32_u: void,
    i64_trunc_f64_s: void,
    i64_trunc_f64_u: void,
    f32_convert_i32_s: void,
    f32_convert_i32_u: void,
    f32_convert_i64_s: void,
    f32_convert_i64_u: void,
    f32_demote_f64: void,
    f64_convert_i32_s: void,
    f64_convert_i32_u: void,
    f64_convert_i64_s: void,
    f64_convert_i64_u: void,
    f64_promote_f32: void,
    i32_reinterpret_f32: void,
    i64_reinterpret_f64: void,
    f32_reinterpret_i32: void,
    f64_reinterpret_i64: void,
    i32_extend8_s: void,
    i32_extend16_s: void,
    i64_extend8_s: void,
    i64_extend16_s: void,
    i64_extend32_s: void,
    i32_trunc_sat_f32_s: void,
    i32_trunc_sat_f32_u: void,
    i32_trunc_sat_f64_s: void,
    i32_trunc_sat_f64_u: void,
    i64_trunc_sat_f32_s: void,
    i64_trunc_sat_f32_u: void,
    i64_trunc_sat_f64_s: void,
    i64_trunc_sat_f64_u: void,
    v128_load: MemArg,
    v128_load_8x8_s: MemArg,
    v128_load_8x8_u: MemArg,
    v128_load_16x4_s: MemArg,
    v128_load_16x4_u: MemArg,
    v128_load_32x2_s: MemArg,
    v128_load_32x2_u: MemArg,
    v128_load8_splat: MemArg,
    v128_load16_splat: MemArg,
    v128_load32_splat: MemArg,
    v128_load64_splat: MemArg,
    v128_load32_zero: MemArg,
    v128_load64_zero: MemArg,
    v128_store: MemArg,
    v128_load8_lane: struct { MemArg, LaneIdx },
    v128_load16_lane: struct { MemArg, LaneIdx },
    v128_load32_lane: struct { MemArg, LaneIdx },
    v128_load64_lane: struct { MemArg, LaneIdx },
    v128_store8_lane: struct { MemArg, LaneIdx },
    v128_store16_lane: struct { MemArg, LaneIdx },
    v128_store32_lane: struct { MemArg, LaneIdx },
    v128_store64_lane: struct { MemArg, LaneIdx },
    v128_const: i128,
    i8x16_shuffle: [16]LaneIdx,
    i8x16_extract_lane_s: LaneIdx,
    i8x16_extract_lane_u: LaneIdx,
    i8x16_replace_lane: LaneIdx,
    i16x8_extract_lane_s: LaneIdx,
    i16x8_extract_lane_u: LaneIdx,
    i16x8_replace_lane: LaneIdx,
    i32x4_extract_lane: LaneIdx,
    i32x4_replace_lane: LaneIdx,
    i64x2_extract_lane: LaneIdx,
    i64x2_replace_lane: LaneIdx,
    f32x4_extract_lane: LaneIdx,
    f32x4_replace_lane: LaneIdx,
    f64x2_extract_lane: LaneIdx,
    f64x2_replace_lane: LaneIdx,
    i8x16_swizzle: void,
    i8x16_splat: void,
    i16x8_splat: void,
    i32x4_splat: void,
    i64x2_splat: void,
    f32x4_splat: void,
    f64x2_splat: void,
    i8x16_eq: void,
    i8x16_ne: void,
    i8x16_lt_s: void,
    i8x16_lt_u: void,
    i8x16_gt_s: void,
    i8x16_gt_u: void,
    i8x16_le_s: void,
    i8x16_le_u: void,
    i8x16_ge_s: void,
    i8x16_ge_u: void,
    i16x8_eq: void,
    i16x8_ne: void,
    i16x8_lt_s: void,
    i16x8_lt_u: void,
    i16x8_gt_s: void,
    i16x8_gt_u: void,
    i16x8_le_s: void,
    i16x8_le_u: void,
    i16x8_ge_s: void,
    i16x8_ge_u: void,
    i32x4_eq: void,
    i32x4_ne: void,
    i32x4_lt_s: void,
    i32x4_lt_u: void,
    i32x4_gt_s: void,
    i32x4_gt_u: void,
    i32x4_le_s: void,
    i32x4_le_u: void,
    i32x4_ge_s: void,
    i32x4_ge_u: void,
    i64x2_eq: void,
    i64x2_ne: void,
    i64x2_lt_s: void,
    i64x2_gt_s: void,
    i64x2_le_s: void,
    i64x2_ge_s: void,
    f32x4_eq: void,
    f32x4_ne: void,
    f32x4_lt: void,
    f32x4_gt: void,
    f32x4_le: void,
    f32x4_ge: void,
    f64x2_eq: void,
    f64x2_ne: void,
    f64x2_lt: void,
    f64x2_gt: void,
    f64x2_le: void,
    f64x2_ge: void,
    v128_not: void,
    v128_and: void,
    v128_andnot: void,
    v128_or: void,
    v128_xor: void,
    v128_bitselect: void,
    v128_any_true: void,
    i8x16_abs: void,
    i8x16_neg: void,
    i8x16_popcnt: void,
    i8x16_all_true: void,
    i8x16_bitmask: void,
    i8x16_narrow_i16x8_s: void,
    i8x16_narrow_i16x8_u: void,
    i8x16_shl: void,
    i8x16_shr_s: void,
    i8x16_shr_u: void,
    i8x16_add: void,
    i8x16_add_sat_s: void,
    i8x16_add_sat_u: void,
    i8x16_sub: void,
    i8x16_sub_sat_s: void,
    i8x16_sub_sat_u: void,
    i8x16_min_s: void,
    i8x16_min_u: void,
    i8x16_max_s: void,
    i8x16_max_u: void,
    i8x16_avgr_u: void,
    i16x8_extadd_pairwise_i8x16_s: void,
    i16x8_extadd_pairwise_i8x16_u: void,
    i16x8_abs: void,
    i16x8_neg: void,
    i16x8_q15mulr_sat_s: void,
    i16x8_all_true: void,
    i16x8_bitmask: void,
    i16x8_narrow_i32x4_s: void,
    i16x8_narrow_i32x4_u: void,
    i16x8_extend_low_i8x16_s: void,
    i16x8_extend_high_i8x16_s: void,
    i16x8_extend_low_i8x16_u: void,
    i16x8_extend_high_i8x16_u: void,
    i16x8_shl: void,
    i16x8_shr_s: void,
    i16x8_shr_u: void,
    i16x8_add: void,
    i16x8_add_sat_s: void,
    i16x8_add_sat_u: void,
    i16x8_sub: void,
    i16x8_sub_sat_s: void,
    i16x8_sub_sat_u: void,
    i16x8_mul: void,
    i16x8_min_s: void,
    i16x8_min_u: void,
    i16x8_max_s: void,
    i16x8_max_u: void,
    i16x8_avgr_u: void,
    i16x8_extmul_low_i8x16_s: void,
    i16x8_extmul_high_i8x16_s: void,
    i16x8_extmul_low_i8x16_u: void,
    i16x8_extmul_high_i8x16_u: void,
    i32x4_extadd_pairwise_i16x8_s: void,
    i32x4_extadd_pairwise_i16x8_u: void,
    i32x4_abs: void,
    i32x4_neg: void,
    i32x4_all_true: void,
    i32x4_bitmask: void,
    i32x4_extend_low_i16x8_s: void,
    i32x4_extend_high_i16x8_s: void,
    i32x4_extend_low_i16x8_u: void,
    i32x4_extend_high_i16x8_u: void,
    i32x4_shl: void,
    i32x4_shr_s: void,
    i32x4_shr_u: void,
    i32x4_add: void,
    i32x4_sub: void,
    i32x4_mul: void,
    i32x4_min_s: void,
    i32x4_min_u: void,
    i32x4_max_s: void,
    i32x4_max_u: void,
    i32x4_dot_i16x8_s: void,
    i32x4_extmul_low_i16x8_s: void,
    i32x4_extmul_high_i16x8_s: void,
    i32x4_extmul_low_i16x8_u: void,
    i32x4_extmul_high_i16x8_u: void,
    i64x2_abs: void,
    i64x2_neg: void,
    i64x2_all_true: void,
    i64x2_bitmask: void,
    i64x2_extend_low_i32x4_s: void,
    i64x2_extend_high_i32x4_s: void,
    i64x2_extend_low_i32x4_u: void,
    i64x2_extend_high_i32x4_u: void,
    i64x2_shl: void,
    i64x2_shr_s: void,
    i64x2_shr_u: void,
    i64x2_add: void,
    i64x2_sub: void,
    i64x2_mul: void,
    i64x2_extmul_low_i32x4_s: void,
    i64x2_extmul_high_i32x4_s: void,
    i64x2_extmul_low_i32x4_u: void,
    i64x2_extmul_high_i32x4_u: void,
    f32x4_ceil: void,
    f32x4_floor: void,
    f32x4_trunc: void,
    f32x4_nearest: void,
    f32x4_abs: void,
    f32x4_neg: void,
    f32x4_sqrt: void,
    f32x4_add: void,
    f32x4_sub: void,
    f32x4_mul: void,
    f32x4_div: void,
    f32x4_min: void,
    f32x4_max: void,
    f32x4_pmin: void,
    f32x4_pmax: void,
    f64x2_ceil: void,
    f64x2_floor: void,
    f64x2_trunc: void,
    f64x2_nearest: void,
    f64x2_abs: void,
    f64x2_neg: void,
    f64x2_sqrt: void,
    f64x2_add: void,
    f64x2_sub: void,
    f64x2_mul: void,
    f64x2_div: void,
    f64x2_min: void,
    f64x2_max: void,
    f64x2_pmin: void,
    f64x2_pmax: void,
    i32x4_trunc_sat_f32x4_s: void,
    i32x4_trunc_sat_f32x4_u: void,
    f32x4_convert_i32x4_s: void,
    f32x4_convert_i32x4_u: void,
    i32x4_trunc_sat_f64x2_s_zero: void,
    i32x4_trunc_sat_f64x2_u_zero: void,
    f64x2_convert_low_i32x4_s: void,
    f64x2_convert_low_i32x4_u: void,
    f32x4_demote_f64x2_zero: void,
    f64x2_promote_low_f32x4: void,
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

    pub fn nextBytes(self: *@This(), len: anytype) !switch (@typeInfo(@TypeOf(len))) {
        .ComptimeInt => *const [len]u8,

        .Int => |info| if (info.signedness != .unsigned)
            @compileError("signed lengths are not supported")
        else
            []const u8,

        else => @compileError("unsupported len type: " ++ @typeName(@TypeOf(len))),
    } {
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
};

test "ref all" {
    std.testing.refAllDeclsRecursive(@This());
}
