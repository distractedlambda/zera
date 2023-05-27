const std = @import("std");
const wasm = @import("wasm.zig");

const ir = @import("arm64/ir.zig");

pub const ModuleCompiler = struct {
    allocator: std.mem.Allocator,

    types: std.ArrayListUnmanaged(wasm.FunctionType) = .{},
    imported_functions: std.ArrayListUnmanaged(ImportedFunction) = .{},
    imported_tables: std.ArrayListUnmanaged(ImportedTable) = .{},
    imported_memories: std.ArrayListUnmanaged(ImportedMemory) = .{},
    imported_globals: std.ArrayListUnmanaged(ImportedGlobal) = .{},
    defined_functions: std.ArrayListUnmanaged(wasm.TypeIndex) = .{},
    defined_tables: std.ArrayListUnmanaged(wasm.TableType) = .{},
    defined_memories: std.ArrayListUnmanaged(wasm.MemoryType) = .{},
    defined_globals: std.ArrayListUnmanaged(wasm.Global) = .{},
    exports: std.ArrayListUnmanaged(wasm.Export) = .{},
    start: ?wasm.FunctionIndex = null,
    data_count: ?u32 = null,

    current_function: wasm.FunctionIndex = 0,

    node_arena: std.heap.ArenaAllocator,
    operand_stack: std.ArrayListUnmanaged(Operand) = .{},
    label_stack: std.ArrayListUnmanaged(Label) = .{},
    locals: std.ArrayListUnmanaged(Local) = .{},
    i32_zero: *ir.I32Const = undefined,
    i64_zero: *ir.I64Const = undefined,
    f32_zero: *ir.F32Const = undefined,
    f64_zero: *ir.F64Const = undefined,
    v128_zero: *ir.V128Const = undefined,
    current_block: *ir.Block = undefined,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .node_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn reset(self: *@This()) void {
        self.types.clearRetainingCapacity();
        self.imported_functions.clearRetainingCapacity();
        self.imported_tables.clearRetainingCapacity();
        self.imported_memories.clearRetainingCapacity();
        self.imported_globals.clearRetainingCapacity();
        self.defined_functions.clearRetainingCapacity();
        self.defined_tables.clearRetainingCapacity();
        self.defined_memories.clearRetainingCapacity();
        self.defined_globals.clearRetainingCapacity();
        self.exports.clearRetainingCapacity();
        self.start = null;
        self.data_count = null;
        self.current_function = 0;
    }

    pub fn deinit(self: *@This()) void {
        self.types.deinit(self.allocator);
        self.imported_functions.deinit(self.allocator);
        self.imported_tables.deinit(self.allocator);
        self.imported_memories.deinit(self.allocator);
        self.imported_globals.deinit(self.allocator);
        self.defined_functions.deinit(self.allocator);
        self.defined_tables.deinit(self.allocator);
        self.defined_memories.deinit(self.allocator);
        self.defined_globals.deinit(self.allocator);
        self.exports.deinit(self.allocator);

        self.node_arena.deinit();
        self.operand_stack.deinit(self.allocator);
        self.label_stack.deinit(self.allocator);
        self.locals.deinit(self.allocator);
    }

    pub fn nodeAllocator(self: *@This()) std.mem.Allocator {
        return self.node_arena.allocator();
    }

    pub fn newNode(self: *@This(), node: anytype) !*@TypeOf(node) {
        const memory = try self.nodeAllocator().create(@TypeOf(node));
        memory.* = node;
        return memory;
    }

    pub fn pushOperand(self: *@This(), value: *ir.Instruction, typ: wasm.ValueType) !void {
        try self.operand_stack.append(self.allocator, .{ .value = value, .type = typ });
    }

    pub fn popOperand(self: *@This()) !Operand {
        return self.operand_stack.popOrNull() orelse error.OperandStackUnderflow;
    }

    pub fn visitCustomSection(self: *@This(), name: []const u8, data: []const u8) !void {
        _ = self;
        _ = name;
        _ = data;
    }

    pub fn visitTypeSection(self: *@This(), len: usize) !*@This() {
        try self.types.ensureUnusedCapacity(self.allocator, len);
        return self;
    }

    pub fn visitType(self: *@This(), _: usize, typ: wasm.FunctionType) !void {
        self.types.appendAssumeCapacity(typ);
    }

    pub fn visitImportSection(self: *@This(), _: usize) !*@This() {
        return self;
    }

    pub fn visitImportedFunction(self: *@This(), func: wasm.ImportedFunction) !void {
        try self.imported_functions.append(self.allocator, .{ .wasm = func });
    }

    pub fn visitImportedTable(self: *@This(), func: wasm.ImportedTable) !void {
        try self.imported_tables.append(self.allocator, .{ .wasm = func });
    }

    pub fn visitImportedMemory(self: *@This(), func: wasm.ImportedMemory) !void {
        try self.imported_memories.append(self.allocator, .{ .wasm = func });
    }

    pub fn visitImportedGlobal(self: *@This(), func: wasm.ImportedGlobal) !void {
        try self.imported_globals.append(self.allocator, .{ .wasm = func });
    }

    pub fn visitFunctionSection(self: *@This(), len: usize) !*@This() {
        try self.functions.ensureUnusedCapacity(self.allocator, len);
        return self;
    }

    pub fn visitFunction(self: *@This(), _: usize, typ: wasm.TypeIndex) !void {
        self.functions.appendAssumeCapacity(typ);
    }

    pub fn visitTableSection(self: *@This(), len: usize) !*@This() {
        try self.tables.ensureUnusedCapacity(self.allocator, len);
        return self;
    }

    pub fn visitTable(self: *@This(), _: usize, typ: wasm.TableType) !void {
        self.tables.appendAssumeCapacity(typ);
    }

    pub fn visitMemorySection(self: *@This(), len: usize) !*@This() {
        try self.memories.ensureUnusedCapacity(self.allocator, len);
        return self;
    }

    pub fn visitMemory(self: *@This(), _: usize, typ: wasm.MemoryType) !void {
        self.memories.appendAssumeCapacity(typ);
    }

    pub fn visitGlobalSection(self: *@This(), len: usize) !*@This() {
        try self.globals.ensureUnusedCapacity(self.allocator, len);
        return self;
    }

    pub fn visitGlobal(self: *@This(), _: usize, global: wasm.Global) !void {
        self.globals.appendAssumeCapacity(global);
    }

    pub fn visitExportSection(self: *@This(), len: usize) !*@This() {
        try self.exports.ensureUnusedCapacity(self.allocator, len);
        return self;
    }

    pub fn visitExport(self: *@This(), _: usize, exp: wasm.Export) !void {
        self.exports.appendAssumeCapacity(exp);
    }

    pub fn visitStartSection(self: *@This(), start: wasm.FunctionIndex) !void {
        self.start = start;
    }

    pub fn visitCodeSection(self: *@This(), _: usize) !*@This() {
        return self;
    }

    pub fn visitFunctionCode(self: *@This(), _: usize) !*@This() {
        if (self.current_function >= self.functions.items.len)
            return error.UndeclaredFunction;

        _ = self.node_arena.reset(self.allocator, .retain_capacity);

        self.operand_stack.clearRetainingCapacity();
        self.label_stack.clearRetainingCapacity();
        self.locals.clearRetainingCapacity();

        self.i32_zero = try self.newNode(ir.I32Const{ .value = 0 });
        self.i64_zero = try self.newNode(ir.I64Const{ .value = 0 });
        self.f32_zero = try self.newNode(ir.F32Const{ .value = 0 });
        self.f64_zero = try self.newNode(ir.F64Const{ .value = 0 });
        self.v128_zero = try self.newNode(ir.V128Const{ .value = 0 });
        self.funcref_null = try self.newNode(ir.Instruction.init(.funcref_null));
        self.externref_null = try self.newNode(ir.Instruction.init(.externref_null));
        self.current_block = try self.newNode(ir.Block{});

        return self;
    }

    pub fn visitLocals(self: *@This(), count: u32, typ: wasm.ValueType) !void {
        const fill = switch (typ) {
            .i32, .funcref => self.i32_zero,
            .i64 => self.i64_zero,
            .f32 => self.f32_zero,
            .f64 => self.f64_zero,
            .v128 => self.v128_zero,
            .externref => @panic("TODO implement externref support"),
        };

        _ = count;
        _ = fill;
    }

    pub fn visitDataCountSection(self: *@This(), count: u32) !void {
        self.data_count = count;
    }

    pub fn visitSelect(self: *@This(), typ: ?wasm.ValueType) !void {
        _ = self;
        _ = typ;
        std.debug.panic("TODO implement select", .{});
    }

    pub fn visitI32Const(self: *@This(), value: i32) !void {
        try self.pushOperand(&(try self.newNode(ir.I32Const{ .value = value })).base, .i32);
    }

    pub fn visitI64Const(self: *@This(), value: i64) !void {
        try self.pushOperand(&(try self.newNode(ir.I64Const{ .value = value })).base, .i64);
    }

    pub fn visitF32Const(self: *@This(), value: f32) !void {
        try self.pushOperand(&(try self.newNode(ir.F32Const{ .value = value })).base, .f32);
    }

    pub fn visitF64Const(self: *@This(), value: f64) !void {
        try self.pushOperand(&(try self.newNode(ir.F64Const{ .value = value })).base, .f64);
    }

    pub fn visitSimpleInstruction(self: *@This(), opcode: wasm.SimpleInstruction) !void {
        switch (opcode) {
            .@"unreachable" => {
                // self.current_block.append(&(try self.newNode(ir.Udf{ .immediate = 0 })).base);
            },

            .nop => {},

            .drop => {
                _ = try self.popOperand();
            },

            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitBlockTypeInstruction(self: *@This(), opcode: wasm.BlockTypeInstruction, block_type: wasm.BlockType) !void {
        _ = self;
        _ = block_type;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitIndexInstruction(self: *@This(), opcode: wasm.IndexInstruction, index: u32) !void {
        _ = self;
        _ = index;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitMemoryInstruction(self: *@This(), opcode: wasm.MemoryInstruction, mem_arg: wasm.MemoryArgument) !void {
        _ = self;
        _ = mem_arg;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitExtendedInstruction(self: *@This(), opcode: wasm.ExtendedInstruction) !void {
        _ = self;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitExtendedIndexInstruction(self: *@This(), opcode: wasm.ExtendedIndexInstruction, index: u32) !void {
        _ = self;
        _ = index;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitExtendedDualIndexInstruction(self: *@This(), opcode: wasm.ExtendedDualIndexInstruction, index_a: u32, index_b: u32) !void {
        _ = self;
        _ = index_a;
        _ = index_b;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitVectorInstruction(self: *@This(), opcode: wasm.VectorInstruction) !void {
        _ = self;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitVectorMemoryInstruction(self: *@This(), opcode: wasm.VectorMemoryInstruction, mem_arg: wasm.MemoryArgument) !void {
        _ = self;
        _ = mem_arg;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitVectorLaneInstruction(self: *@This(), opcode: wasm.VectorLaneInstruction, lane_idx: wasm.LaneIndex) !void {
        _ = self;
        _ = lane_idx;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }

    pub fn visitVectorMemoryLaneInstruction(self: *@This(), opcode: wasm.VectorMemoryLaneInstruction, mem_arg: wasm.MemoryArgument, lane_idx: wasm.LaneIndex) !void {
        _ = self;
        _ = mem_arg;
        _ = lane_idx;
        switch (opcode) {
            else => std.debug.panic("TODO implement {}", .{opcode}),
        }
    }
};

const ImportedFunction = struct {
    wasm: wasm.ImportedFunction,
    vtable_offset: usize = undefined,
};

const ImportedTable = struct {
    wasm: wasm.ImportedFunction,
    ptr_offset: usize = undefined,
};

const ImportedMemory = struct {
    wasm: wasm.ImportedMemory,
    ptr_offset: usize = undefined,
};

const ImportedGlobal = struct {
    wasm: wasm.ImportedGlobal,
    ptr_offset: usize = undefined,
};

const ImportedFunctionVTable = extern struct {
    context: *anyopaque,
    impl: *fn (context: *anyopaque, in: *const anyopaque, out: *anyopaque) callconv(.C) void,
};

const Table = extern struct {
    contents: ?[*]u32,
    size: u32,
};

const Memory = extern struct {
    base: usize,
    size: u32,
};

const Operand = struct {
    value: *ir.Instruction,
    type: wasm.ValueType,
};

const Label = struct {
    block: *ir.Block,
};

const Local = struct {
    value: *ir.Instruction,
    type: wasm.ValueType,
};

pub fn compile(module_data: []const u8, allocator: std.mem.Allocator) !void {
    var compiler = ModuleCompiler.init(allocator);
    try wasm.visitModule(module_data, &compiler);
}

test "ref all" {
    std.testing.refAllDeclsRecursive(@This());
}
