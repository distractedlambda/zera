const std = @import("std");
const wasm = @import("../wasm.zig");

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

pub fn nextName(self: *@This()) !wasm.Name {
    return try self.nextBytes(try self.nextInt(u32));
}

pub fn nextNumericType(self: *@This()) !wasm.NumericType {
    return std.meta.intToEnum(wasm.NumericType, try self.nextByte()) catch
        error.UnsupportedNumberType;
}

pub fn nextVectorType(self: *@This()) !wasm.VectorType {
    return std.meta.intToEnum(wasm.VectorType, try self.nextByte()) catch
        error.UnsupportedVectorType;
}

pub fn nextReferenceType(self: *@This()) !wasm.ReferenceType {
    return std.meta.intToEnum(wasm.ReferenceType, try self.nextByte()) catch
        error.UnsupportedReferenceType;
}

pub fn nextValueType(self: *@This()) !wasm.ValueType {
    return std.meta.intToEnum(wasm.ValueType, try self.nextByte()) catch
        error.UnsupportedValueType;
}

pub fn nextResultType(self: *@This()) !wasm.ResultType {
    const len = try self.nextInt(u32);
    const types = try self.nextBytes(len);

    for (types) |t|
        _ = std.meta.intToEnum(wasm.ValueType, t) catch
            return error.UnsupportedValueType;

    return std.mem.bytesAsSlice(wasm.ValueType, types);
}

pub fn nextFunctionType(self: *@This()) !wasm.FunctionType {
    if (try self.nextByte() != 0x60)
        return error.UnsupportedFunctionType;

    return .{
        .parameters = try self.nextResultType(),
        .results = try self.nextResultType(),
    };
}

pub fn nextLimits(self: *@This()) !wasm.Limits {
    return switch (try self.nextByte()) {
        0x00 => .{ .min = try self.nextInt(u32) },
        0x01 => .{ .min = try self.nextInt(u32), .max = try self.nextInt(u32) },
        else => error.UnsupportedLimits,
    };
}

pub fn nextMemoryType(self: *@This()) !wasm.MemoryType {
    return .{ .limits = try self.nextLimits() };
}

pub fn nextTableType(self: *@This()) !wasm.TableType {
    return .{
        .element_type = try self.nextReferenceType(),
        .limits = try self.nextLimits(),
    };
}

pub fn nextMutability(self: *@This()) !wasm.Mutability {
    return std.meta.intToEnum(wasm.Mutability, try self.nextByte()) catch
        error.UnsupportedMutability;
}

pub fn nextElemKind(self: *@This()) !wasm.ElemKind {
    return std.meta.intToEnum(wasm.ElemKind, try self.nextByte()) catch
        error.UnsupportedElemKind;
}

pub fn nextGlobalType(self: *@This()) !wasm.GlobalType {
    return .{
        .value_type = try self.nextValueType(),
        .mutability = try self.nextMutability(),
    };
}
