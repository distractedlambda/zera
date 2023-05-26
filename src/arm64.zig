const std = @import("std");
const wasm = @import("wasm.zig");

const ir = @import("arm64/ir.zig");

const ModuleCompiler = struct {
    allocator: std.mem.Allocator,

    types: std.ArrayListUnmanaged(wasm.FuncType) = .{},
    imports: std.ArrayListUnmanaged(wasm.Import) = .{},
    functions: std.ArrayListUnmanaged(Function) = .{},
    tables: std.ArrayListUnmanaged(wasm.TableType) = .{},
    memories: std.ArrayListUnmanaged(wasm.MemType) = .{},
    globals: std.ArrayListUnmanaged(wasm.Global) = .{},
    exports: std.ArrayListUnmanaged(wasm.Export) = .{},
    start: ?wasm.FuncIdx = null,

    function_arena: std.heap.ArenaAllocator,
    operand_stack: std.ArrayListUnmanaged(Operand) = .{},
    label_stack: std.ArrayListUnmanaged(Label) = .{},
    locals: std.ArrayListUnmanaged(Local) = .{},

    fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .function_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    fn resetFunction(self: *@This()) void {
        _ = self.function_arena.reset(.retain_capacity);
        self.operand_stack.clearRetainingCapacity();
        self.label_stack.clearRetainingCapacity();
        self.locals.clearRetainingCapacity();
    }

    fn reset(self: *@This()) void {
        self.types.clearRetainingCapacity();
        self.imports.clearRetainingCapacity();
        self.functions.clearRetainingCapacity();
        self.tables.clearRetainingCapacity();
        self.memories.clearRetainingCapacity();
        self.globals.clearRetainingCapacity();
        self.exports.clearRetainingCapacity();

        self.resetFunction();
    }

    fn deinit(self: *@This()) void {
        self.types.deinit(self.allocator);
        self.imports.deinit(self.allocator);
        self.functions.deinit(self.allocator);
        self.tables.deinit(self.allocator);
        self.memories.deinit(self.allocator);
        self.globals.deinit(self.allocator);
        self.exports.deinit(self.allocator);

        self.function_arena.deinit();
        self.operand_stack.deinit(self.allocator);
        self.label_stack.deinit(self.allocator);
        self.locals.deinit(self.allocator);
    }

    fn functionAllocator(self: *@This()) std.mem.Allocator {
        return self.function_arena.allocator();
    }
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
};

const Function = struct {
    type: wasm.TypeIdx,
};

const ModuleVisitor = struct {
    compiler: *ModuleCompiler,

    fn visitCustomSection(self: @This(), name: []const u8, data: []const u8) void {
        _ = self;
        _ = name;
        _ = data;
    }

    fn visitTypeSection(self: @This(), len: usize) !TypeSectionVisitor {
        try self.compiler.types.ensureUnusedCapacity(self.compiler.allocator, len);
        return .{ .compiler = self.compiler };
    }

    fn visitImportSection(self: @This(), len: usize) !ImportSectionVisitor {
        try self.compiler.imports.ensureUnusedCapacity(self.compiler.allocator, len);
        return .{ .compiler = self.compiler };
    }
};

const TypeSectionVisitor = struct {
    compiler: *ModuleCompiler,

    fn visitType(self: @This(), _: u32, typ: wasm.FuncType) void {
        self.compiler.types.appendAssumeCapacity(typ);
    }
};

const ImportSectionVisitor = struct {
    compiler: *ModuleCompiler,

    fn visitImport(self: @This(), _: u32, import: wasm.Import) void {
        self.compiler.imports.appendAssumeCapacity(import);
    }
};

const InstructionVisitor = struct {
    compiler: *ModuleCompiler,

    fn visitSelect(self: @This(), typ: ?wasm.ValueType) !void {}

    fn visitI32Const(self: @This(), value: i32) !void {}

    fn visitI64Const(self: @This(), value: i64) !void {}

    fn visitF32Const(self: @This(), value: f32) !void {}

    fn visitF64Const(self: @This(), value: f64) !void {}

    fn visitSimpleInstruction(self: @This(), opcode: wasm.SimpleInstruction) !void {}

    fn visitBlockTypeInstruction(self: @This(), opcode: wasm.BlockTypeInstruction, block_type: wasm.BlockType) !void {}

    fn visitIndexInstruction(self: @This(), opcode: wasm.IndexInstruction, index: u32) !void {}

    fn visitMemoryInstruction(self: @This(), opcode: wasm.MemoryInstruction, mem_arg: wasm.MemoryArgument) !void {}

    fn visitExtendedInstruction(self: @This(), opcode: wasm.ExtendedInstruction) !void {}

    fn visitExtendedIndexInstruction(self: @This(), opcode: wasm.ExtendedIndexInstruction, index: u32) !void {}

    fn visitExtendedDualIndexInstruction(self: @This(), opcode: wasm.ExtendedDualIndexInstruction, index_a: u32, index_b: u32) !void {}

    fn visitVectorInstruction(self: @This(), opcode: wasm.VectorInstruction) !void {}

    fn visitVectorMemoryInstruction(self: @This(), opcode: wasm.VectorMemoryInstruction, mem_arg: wasm.MemoryArgument) !void {}

    fn visitVectorLaneInstruction(self: @This(), opcode: wasm.VectorLaneInstruction, lane_idx: wasm.LaneIndex) !void {}

    fn visitVectorMemoryLaneInstruction(self: @This(), opcode: wasm.VectorMemoryLaneInstruction, mem_arg: wasm.MemoryArgument, lane_idx: wasm.LaneIndex) !void {}
};
