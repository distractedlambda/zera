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
    i32_const: u32,
    i64_const: u64,
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
    inital_value: ConstantExpr,
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
                if (b > std.math.MaxInt(std.meta.Int(.unsigned, remaining_bits)))
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
                        @truncate(u7, accum),
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
            error.InvalidNumberType;
    }

    pub fn nextVecType(self: *@This()) !VecType {
        return std.meta.intToEnum(VecType, try self.nextByte()) catch
            error.InvalidVectorType;
    }

    pub fn nextRefType(self: *@This()) !RefType {
        return std.meta.intToEnum(RefType, try self.nextByte()) catch
            error.InvalidReferenceType;
    }

    pub fn nextValType(self: *@This()) !ValType {
        return std.meta.intToEnum(ValType, try self.nextByte()) catch
            error.InvalidValueType;
    }

    pub fn nextResultType(self: *@This()) !ResultType {
        const len = try self.nextInt(u32);
        const types = try self.nextBytes(len);

        for (types) |t|
            _ = std.meta.intToEnum(ValType, t) catch
                return error.InvalidValueType;

        return std.mem.bytesAsSlice(ValType, types);
    }

    pub fn nextFuncType(self: *@This()) !FuncType {
        if (try self.nextByte() != 0x60)
            return error.InvalidFunctionType;

        return .{
            .parameters = try self.nextResultType(),
            .results = try self.nextResultType(),
        };
    }

    pub fn nextLimits(self: *@This()) !Limits {
        return switch (try self.nextByte()) {
            0x00 => .{ .min = try self.nextInt(u32) },
            0x01 => .{ .min = try self.nextInt(u32), .max = try self.nextInt(u32) },
            else => error.InvalidLimits,
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
            error.InvalidMutability;
    }

    pub fn nextElemKind(self: *@This()) !ElemKind {
        return std.meta.intToEnum(ElemKind, try self.nextByte()) catch
            error.InvalidElemKind;
    }

    pub fn nextGlobalType(self: *@This()) !GlobalType {
        return .{
            .value_type = try self.nextValType(),
            .mutability = try self.nextMut(),
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
                if (index < 0) return error.InvalidBlockType;
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
            error.InvalidSectionId;
    }

    pub fn nextImportDesc(self: *@This()) !ImportDesc {
        return switch (try self.nextByte()) {
            0x00 => .{ .function = try self.nextInt(FuncIdx) },
            0x01 => .{ .table = try self.nextTableType() },
            0x02 => .{ .memory = try self.nextMemType() },
            0x03 => .{ .global = try self.nextGlobalType() },
            else => error.InvalidImportDescriptor,
        };
    }

    pub fn nextImport(self: *@This()) !Import {
        return .{
            .module_name = try self.nextName(),
            .name = try self.nextName(),
            .descriptor = try self.nextImportDesc(),
        };
    }

    pub fn nextConstantExpr(self: *@This()) !ConstantExpr {
        const value = switch (try self.nextByte()) {
            0x23 => .{ .global = try self.nextInt(GlobalIdx) },
            0x41 => .{ .i32 = try self.nextInt(i32) },
            0x42 => .{ .i64 = try self.nextInt(i64) },
            0x43 => .{ .f32 = try self.nextFloat(f32) },
            0x44 => .{ .f64 = try self.nextFloat(f64) },
            0xd0 => .null_ref,
            0xd2 => .{ .function_ref = try self.nextInt(FuncIdx) },
            else => return error.InvalidConstantExpression,
        };

        if (try self.nextByte() != 0x0b)
            return error.InvalidConstantExpression;

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
            0x00 => .{ .function = try self.nextInt(FuncIdx) },
            0x01 => .{ .table = try self.nextInt(TableIdx) },
            0x02 => .{ .memory = try self.nextInt(MemIdx) },
            0x03 => .{ .global = try self.nextInt(GlobalIdx) },
        };
    }

    pub fn nextExport(self: *@This()) !Export {
        return .{
            .name = try self.nextName(),
            .descriptor = try self.nextExportDesc(),
        };
    }
};
