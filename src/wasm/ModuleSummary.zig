const opcodes = @import("opcodes.zig");
const std = @import("std");
const wasm = @import("../wasm.zig");

const Decoder = @import("Decoder.zig");

types: std.ArrayListUnmanaged(wasm.FunctionType) = .{},
imported_functions: std.ArrayListUnmanaged(wasm.ImportedFunction) = .{},
imported_tables: std.ArrayListUnmanaged(wasm.ImportedTable) = .{},
imported_memories: std.ArrayListUnmanaged(wasm.ImportedMemory) = .{},
imported_globals: std.ArrayListUnmanaged(wasm.ImportedGlobal) = .{},
functions: std.ArrayListUnmanaged(wasm.TypeIndex) = .{},
tables: std.ArrayListUnmanaged(wasm.TableType) = .{},
memories: std.ArrayListUnmanaged(wasm.MemoryType) = .{},
globals: std.ArrayListUnmanaged(wasm.Global) = .{},
exports: std.ArrayListUnmanaged(wasm.Export) = .{},
start: ?wasm.FunctionIndex = null,
element_segments: std.ArrayListUnmanaged(wasm.ElementSegment) = .{},
code: std.ArrayListUnmanaged([]const u8) = .{},
data_segments: std.ArrayListUnmanaged(wasm.DataSegment) = .{},
module_name: ?[]const u8 = null,
function_names: std.AutoHashMapUnmanaged(wasm.FunctionIndex, []const u8) = .{},
local_names: std.AutoHashMapUnmanaged(struct { wasm.FunctionIndex, wasm.LocalIndex }, []const u8) = .{},

pub fn init(allocator: std.mem.Allocator, module_data: []const u8) !@This() {
    var directory = @This(){};
    errdefer directory.deinit(allocator);
    try directory.processModule(allocator, module_data);
    return directory;
}

pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    for (self.element_segments.items) |*segment| {
        switch (segment.init) {
            inline else => |vector| allocator.free(vector),
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

    if (!std.mem.eql(u8, &.{ 0x00, 0x61, 0x73, 0x6d }, try decoder.nextBytes(4)))
        return error.NotABinaryWasmModule;

    if (!std.mem.eql(u8, &.{ 0x01, 0x00, 0x00, 0x00 }, try decoder.nextBytes(4)))
        return error.UnsupportedBinaryFormatVersion;

    while (!decoder.atEnd()) {
        const section_id = try decoder.nextByte();
        const section_len = try decoder.nextInt(u32);
        const section_data = try decoder.nextBytes(section_len);
        try self.processSection(allocator, section_id, section_data);
    }

    if (self.functions.items.len != self.code.items.len)
        return error.InconsistentFunctionCount;
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
                const name = wasm.ImportName{
                    .module = try decoder.nextName(),
                    .name = try decoder.nextName(),
                };

                switch (try decoder.nextByte()) {
                    0x00 => try self.imported_functions.append(allocator, .{
                        .name = name,
                        .type = try self.nextIndex(&decoder, wasm.TypeIndex),
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
            for (0..len) |_| self.functions.appendAssumeCapacity(try self.nextIndex(&decoder, wasm.TypeIndex));
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
            self.start = try self.nextIndex(&decoder, wasm.FunctionIndex);
        },

        9 => {
            const len = try decoder.nextInt(u32);
            try self.element_segments.ensureUnusedCapacity(allocator, len);
            for (0..len) |_| self.element_segments.appendAssumeCapacity(try self.nextElementSegment(allocator, &decoder));
        },

        10 => {
            const len = try decoder.nextInt(u32);
            try self.code.ensureUnusedCapacity(allocator, len);
            for (0..len) |_| self.code.appendAssumeCapacity(try decoder.nextBytes(try decoder.nextInt(u32)));
        },

        11 => {
            const len = try decoder.nextInt(u32);
            try self.data_segments.ensureUnusedCapacity(allocator, len);
            for (0..len) |_| self.data_segments.appendAssumeCapacity(try self.nextDataSegment(&decoder));
        },

        12 => {
            // Ignore data count section
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
                const function = try self.nextIndex(&decoder, wasm.FunctionIndex);
                const function_name = try decoder.nextName();
                self.function_names.putAssumeCapacity(function, function_name);
            }
        },

        2 => {
            const outer_len = try decoder.nextInt(u32);
            for (0..outer_len) |_| {
                const function = try self.nextIndex(&decoder, wasm.FunctionIndex);
                const inner_len = try decoder.nextInt(u32);
                try self.local_names.ensureUnusedCapacity(allocator, inner_len);
                for (0..inner_len) |_| {
                    const local = wasm.LocalIndex{ .value = try decoder.nextInt(u32) };
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
        wasm.TypeIndex => if (idx.value >= self.types.items.len)
            return error.TypeIndexOutOfBounds,

        wasm.FunctionIndex => if (idx.value >= self.imported_functions.items.len and idx.value - self.imported_functions.items.len >= self.functions.items.len)
            return error.FunctionIndexOutOfBounds,

        wasm.TableIndex => if (idx.value >= self.imported_tables.items.len and idx.value - self.imported_tables.items.len >= self.tables.items.len)
            return error.TableIndexOutOfBounds,

        wasm.MemoryIndex => if (idx.value >= self.imported_memories.items.len and idx.value - self.imported_memories.items.len >= self.memories.items.len)
            return error.MemoryIndexOutOfBounds,

        wasm.GlobalIndex => if (idx.value >= self.imported_globals.items.len and idx.value - self.imported_globals.items.len >= self.globals.items.len)
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
        wasm.FunctionIndex => self.imported_functions.items.len,
        wasm.TableIndex => self.imported_tables.items.len,
        wasm.MemoryIndex => self.imported_memories.items.len,
        wasm.GlobalIndex => self.imported_globals.items.len,
        else => unreachable,
    };

    return idx.value < imports_len;
}

fn nextConstantGlobalGet(self: *@This(), decoder: *Decoder, expected_type: wasm.ValueType) !wasm.GlobalIndex {
    const idx = try self.nextIndex(decoder, wasm.GlobalIndex);

    if (!self.isImport(idx))
        return error.UnsupportedConstantExpression;

    if (self.imported_globals.items[idx.value].type.value_type != expected_type)
        return error.TypeMismatch;

    return idx;
}

fn nextConstantExpression(self: *@This(), decoder: *Decoder, expected_type: wasm.ValueType) !wasm.ConstantExpression {
    const value: wasm.ConstantExpression = switch (try decoder.nextByte()) {
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
            .{ .ref_func = try self.nextIndex(decoder, wasm.FunctionIndex) }
        else
            return error.TypeMismatch,

        else => return error.UnsupportedConstantExpression,
    };

    if (try decoder.nextByte() != opcodes.end)
        return error.UnsupportedConstantExpression;

    return value;
}

fn nextGlobal(self: *@This(), decoder: *Decoder) !wasm.Global {
    const typ = try decoder.nextGlobalType();
    return .{
        .type = typ,
        .initial_value = try self.nextConstantExpression(decoder, typ.value_type),
    };
}

fn nextExport(self: *@This(), decoder: *Decoder) !wasm.Export {
    return .{
        .name = try decoder.nextName(),

        .desc = switch (try decoder.nextByte()) {
            0x00 => .{ .function = try self.nextIndex(decoder, wasm.FunctionIndex) },
            0x01 => .{ .table = try self.nextIndex(decoder, wasm.TableIndex) },
            0x02 => .{ .memory = try self.nextIndex(decoder, wasm.MemoryIndex) },
            0x03 => .{ .global = try self.nextIndex(decoder, wasm.GlobalIndex) },
            else => return error.UnsupportedExport,
        },
    };
}

fn nextI32ConstantExpression(self: *@This(), decoder: *Decoder) !wasm.I32ConstantExpression {
    const value: wasm.I32ConstantExpression = switch (try decoder.nextByte()) {
        opcodes.@"global.get" => .{ .global_get = try self.nextConstantGlobalGet(decoder, .i32) },
        opcodes.@"i32.const" => .{ .i32_const = try decoder.nextInt(i32) },
        else => return error.UnsupportedI32ConstantExpression,
    };

    if (try decoder.nextByte() != opcodes.end)
        return error.UnsupportedI32ConstantExpression;

    return value;
}

fn nextFuncrefConstantExpression(self: *@This(), decoder: *Decoder) !wasm.FuncrefConstantExpression {
    const value: wasm.FuncrefConstantExpression = switch (try decoder.nextByte()) {
        opcodes.@"global.get" => .{ .global_get = try self.nextConstantGlobalGet(decoder, .funcref) },
        opcodes.@"ref.null" => .ref_null,
        opcodes.@"ref.func" => .{ .ref_func = try self.nextIndex(decoder, wasm.FunctionIndex) },
        else => return error.UnsupportedFuncrefConstantExpression,
    };

    if (try decoder.nextByte() != opcodes.end)
        return error.UnsupportedFuncrefConstantExpression;

    return value;
}

fn nextExternrefConstantExpression(self: *@This(), decoder: *Decoder) !wasm.ExternrefConstantExpression {
    const value: wasm.ExternrefConstantExpression = switch (try decoder.nextByte()) {
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

fn nextFuncrefConstantExpressionVector(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) ![]const wasm.FuncrefConstantExpression {
    const len = try decoder.nextInt(u32);
    const exprs = try allocator.alloc(wasm.FuncrefConstantExpression, len);
    errdefer allocator.free(exprs);
    for (exprs) |*expr| expr.* = try self.nextFuncrefConstantExpression(decoder);
    return exprs;
}

fn nextExternrefConstantExpressionVector(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) ![]const wasm.ExternrefConstantExpression {
    const len = try decoder.nextInt(u32);
    const exprs = try allocator.alloc(wasm.ExternrefConstantExpression, len);
    errdefer allocator.free(exprs);
    for (exprs) |*expr| expr.* = try self.nextExternrefConstantExpression(decoder);
    return exprs;
}

fn nextKindedElementSegmentInit(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) !wasm.ElementSegment.Init {
    return switch (try decoder.nextElemKind()) {
        .funcref => .{ .funcrefs = try self.nextIndexVector(allocator, decoder, wasm.FunctionIndex) },
    };
}

fn nextTypedElementSegmentInit(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) !wasm.ElementSegment.Init {
    return switch (try decoder.nextReferenceType()) {
        .funcref => .{ .funcref_exprs = try self.nextFuncrefConstantExpressionVector(allocator, decoder) },
        .externref => .{ .externref_exprs = try self.nextExternrefConstantExpressionVector(allocator, decoder) },
    };
}

fn nextElementSegment(self: *@This(), allocator: std.mem.Allocator, decoder: *Decoder) !wasm.ElementSegment {
    return switch (try decoder.nextInt(u32)) {
        0 => .{
            .mode = .{ .active = .{
                .table = try self.validateIndex(wasm.TableIndex{ .value = 0 }),
                .offset = try self.nextI32ConstantExpression(decoder),
            } },

            .init = .{ .funcrefs = try self.nextIndexVector(allocator, decoder, wasm.FunctionIndex) },
        },

        1 => .{
            .mode = .passive,
            .init = try self.nextKindedElementSegmentInit(allocator, decoder),
        },

        2 => .{
            .mode = .{ .active = .{
                .table = try self.nextIndex(decoder, wasm.TableIndex),
                .offset = try self.nextI32ConstantExpression(decoder),
            } },

            .init = try self.nextKindedElementSegmentInit(allocator, decoder),
        },

        3 => .{
            .mode = .declarative,
            .init = try self.nextKindedElementSegmentInit(allocator, decoder),
        },

        4 => .{
            .mode = .{ .active = .{
                .table = try self.validateIndex(wasm.TableIndex{ .value = 0 }),
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
                .table = try self.nextIndex(decoder, wasm.TableIndex),
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

fn nextDataSegment(self: *@This(), decoder: *Decoder) !wasm.DataSegment {
    const mode: wasm.DataSegment.Mode = switch (try decoder.nextInt(u32)) {
        0 => .{ .active = .{
            .memory = try self.validateIndex(wasm.MemoryIndex{ .value = 0 }),
            .offset = try self.nextI32ConstantExpression(decoder),
        } },

        1 => .passive,

        2 => .{ .active = .{
            .memory = try self.nextIndex(decoder, wasm.MemoryIndex),
            .offset = try self.nextI32ConstantExpression(decoder),
        } },

        else => return error.UnsupportedDataSegment,
    };

    return .{
        .mode = mode,
        .init = try decoder.nextByteVector(),
    };
}

test "ref all decls" {
    std.testing.refAllDecls(@This());
}

test "summarize test modules" {
    for (@import("../test_modules/manifest.zig").module_names) |name| {
        for (std.meta.tags(std.builtin.OptimizeMode)) |mode| {
            const module_path = try std.fmt.allocPrint(
                std.testing.allocator,
                "test_modules/{s}-{s}.wasm",
                .{ name, @tagName(mode) },
            );
            defer std.testing.allocator.free(module_path);

            const mapping = try @import("../ReadOnlyFileMapping.zig").open(module_path);
            defer mapping.deinit();

            var summary = try init(std.testing.allocator, mapping.contents);
            defer summary.deinit(std.testing.allocator);
        }
    }
}
