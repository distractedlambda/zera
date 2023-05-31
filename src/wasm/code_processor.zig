const opcodes = @import("opcodes.zig");
const std = @import("std");
const wasm = @import("../wasm.zig");

const Decoder = @import("Decoder.zig");
const ModuleSummary = @import("ModuleSummary.zig");

pub fn CodeProcessor(comptime Backend: type) type {
    return struct {
        summary: *const ModuleSummary,
        allocator: std.mem.Allocator,
        locals: std.ArrayListUnmanaged(Local) = .{},
        operand_stack: std.ArrayListUnmanaged(Operand) = .{},
        label_stack: std.ArrayListUnmanaged(Label) = .{},

        const Local = struct {
            type: wasm.ValType,
            backend: BackendLocal,
        };

        const Operand = struct {
            type: wasm.ValType,
            backend: BackendOperand,
        };

        const Label = struct {
            type: wasm.FuncType,
            min_operand_stack_depth: usize,

            backend: union(enum) {
                block: BackendBlock,
                loop: BackendLoop,
                @"if": BackendIf,
                @"else": BackendElse,
            },
        };

        const BackendOperand = getBackendType("Operand");
        const BackendLocal = getBackendType("Local");
        const BackendBlock = getBackendType("Block");
        const BackendLoop = getBackendType("Loop");
        const BackendIf = getBackendType("If");
        const BackendElse = getBackendType("Else");
        const BackendLocalVisitor = getBackendVisitorType("visitLocals");
        const BackendInstrVisitor = getBackendVisitorType("visitInstrs");

        fn getBackendType(comptime name: []const u8) type {
            return if (@hasDecl(Backend, name))
                @field(Backend, name)
            else
                void;
        }

        fn getBackendVisitorType(comptime name: []const u8) type {
            return @typeInfo(@typeInfo(@TypeOf(@field(Backend, name))).Fn.return_type.?).ErrorUnion.payload;
        }

        pub fn init(module_summary: *const ModuleSummary, allocator: std.mem.Allocator) @This() {
            return .{ .summary = module_summary, .allocator = allocator };
        }

        pub fn reset(self: *@This()) void {
            self.locals.clearRetainingCapacity();
            self.operand_stack.clearRetainingCapacity();
            self.label_stack.clearRetainingCapacity();
            self.outermost_rest_unreachable = false;
        }

        pub fn deinit(self: *@This()) void {
            self.locals.deinit(self.allocator);
            self.operand_stack.deinit(self.allocator);
            self.label_stack.deinit(self.allocator);
        }

        pub fn process(self: *@This(), backend: *Backend, func: wasm.FuncIdx, code: []const u8) !void {
            var decoder = Decoder.init(code);

            try self.visitLocals(backend, func, &decoder);

            var instr_visitor = try backend.visitInstrs();
            defer instr_visitor.deinit();
            try self.visitInstrs(&instr_visitor, &decoder);
            try instr_visitor.finish();
        }

        fn visitLocals(self: *@This(), backend: *Backend, func: wasm.FuncIdx, decoder: *Decoder) !void {
            var visitor = try backend.visitLocals();
            defer visitor.deinit();

            // Visit local variables for function parameters
            const func_type = self.summary.lookUpType(func);
            try self.locals.ensureUnusedCapacity(self.allocator, func_type.parameters.len);
            for (func_type.parameters) |t| try self.visitLocal(&visitor, func, t);

            // Visit other local variables
            for (0..try decoder.nextInt(u32)) |_| {
                const n_locals = try decoder.nextInt(u32);
                const local_type = try decoder.nextValType();
                try self.locals.ensureUnusedCapacity(self.allocator, n_locals);
                for (0..n_locals) |_| try self.visitLocal(&visitor, func, local_type);
            }

            try visitor.finish();
        }

        fn visitLocal(self: *@This(), visitor: *BackendLocalVisitor, func: wasm.FuncIdx, typ: wasm.ValType) !void {
            const idx = wasm.LocalIdx{
                .value = std.math.cast(u32, self.locals.items.len) orelse
                    return error.TooManyLocals,
            };

            const name = self.summary.local_names.get(.{ func, idx });

            self.locals.appendAssumeCapacity(.{
                .type = typ,
                .backend = try visitor.visitLocal(typ, idx, name),
            });
        }

        fn visitInstrs(self: *@This(), visitor: *BackendInstrVisitor, decoder: *Decoder) !void {
            while (true) switch (try decoder.nextByte()) {
                opcodes.@"unreachable" => {
                    try visitor.visitUnreachable();
                    if (self.label_stack.items.len == 0) return;
                    try self.skipUnreachable(visitor, decoder);
                },

                opcodes.nop => {},

                opcodes.block => {
                    const ty = try self.nextBlockType(decoder);
                    try self.checkOperandStackTypes(ty.parameters);
                    try self.label_stack.append(self.allocator, .{
                        .type = ty,
                        .min_operand_stack_depth = self.operand_stack.items.len - ty.parameters.len,
                        .backend = .{ .block = try visitor.visitBlock(ty.results) },
                    });
                },

                else => return error.UnsupportedOpcode,
            };
        }

        fn skipUnreachable(self: *@This(), visitor: *BackendInstrVisitor, decoder: *Decoder) !void {
            var additional_label_depth: usize = 0;
            while (true) switch (try decoder.nextByte()) {
                opcodes.block,
                opcodes.loop,
                opcodes.@"if",
                => {
                    _ = try self.nextBlockType(decoder);
                    additional_label_depth = try std.math.add(usize, additional_label_depth, 1);
                },

                opcodes.@"else" => if (additional_label_depth == 0) {
                    const l = self.label_stack.pop();
                    switch (l.backend) {
                        .@"if" => |b| {
                            var arg_iterator = try visitor.visitElse(b);
                            defer arg_iterator.deinit();

                            self.operand_stack.shrinkRetainingCapacity(l.min_operand_stack_depth);
                            for (l.type.parameters) |t| self.operand_stack.appendAssumeCapacity(.{
                                .type = t,
                                .backend = try arg_iterator.next(),
                            });

                            try arg_iterator.finish();
                        },

                        else => return error.UnbalancedElse,
                    }
                },

                opcodes.end => if (additional_label_depth == 0) {
                    // TODO
                } else {
                    additional_label_depth -= 1;
                },

                opcodes.br_table => {
                    for (0..try decoder.nextInt(u32)) |_| _ = try decoder.nextInt(u32);
                    _ = try decoder.nextInt(u32);
                },

                opcodes.@"ref.null" => {
                    // Using nextRefType() instead of nextByte() to avoid sudden
                    // breakage should multi-byte reference types be introduced
                    _ = try decoder.nextRefType();
                },

                opcodes.@"select t" => {
                    // Using nextValType() instead of nextByte() to avoid sudden
                    // breakage should multi-byte value types be introduced
                    for (0..try decoder.nextInt(u32)) |_| _ = try decoder.nextValType();
                },

                // Single-byte opcodes with no immediates
                opcodes.@"unreachable",
                opcodes.nop,
                opcodes.@"return",
                opcodes.@"ref.is_null",
                opcodes.drop,
                opcodes.select,
                => {},

                // Single-byte opcodes with a single u32 immediate
                opcodes.br,
                opcodes.br_if,
                opcodes.call,
                opcodes.@"ref.func",
                opcodes.@"local.get",
                opcodes.@"local.set",
                opcodes.@"local.tee",
                opcodes.@"global.get",
                opcodes.@"global.set",
                opcodes.@"table.get",
                opcodes.@"table.set",
                => {
                    _ = try decoder.nextInt(u32);
                },

                // Single-byte opcodes with two u32 immediates
                opcodes.call_indirect,
                opcodes.@"i32.load",
                opcodes.@"i64.load",
                opcodes.@"f32.load",
                opcodes.@"f64.load",
                opcodes.@"i32.load8_s",
                opcodes.@"i32.load8_u",
                opcodes.@"i32.load16_s",
                opcodes.@"i32.load16_u",
                opcodes.@"i64.load8_s",
                opcodes.@"i64.load8_u",
                opcodes.@"i64.load16_s",
                opcodes.@"i64.load16_u",
                opcodes.@"i64.load32_s",
                opcodes.@"i64.load32_u",
                opcodes.@"i32.store",
                opcodes.@"i64.store",
                opcodes.@"f32.store",
                opcodes.@"f64.store",
                opcodes.@"i32.store8",
                opcodes.@"i32.store16",
                opcodes.@"i64.store8",
                opcodes.@"i64.store16",
                opcodes.@"i64.store32",
                => {
                    _ = try decoder.nextInt(u32);
                    _ = try decoder.nextInt(u32);
                },

                else => return error.UnsupportedOpcode,
            };
        }

        fn nextBlockType(self: *@This(), decoder: *Decoder) !wasm.FuncType {
            return switch (try decoder.peekByte()) {
                0x40 => .{
                    .parameters = &.{},
                    .results = &.{},
                },

                inline 0x6f, 0x70, 0x7b...0x7f => |b| comptime .{
                    .parameters = &.{},
                    .results = &.{@intToEnum(wasm.ValType, b)},
                },

                else => self.summary.lookUp(
                    try self.summary.validateIdx(
                        wasm.TypeIdx{
                            .value = std.math.cast(u32, try decoder.nextInt(i33)) orelse
                                return error.TypeIndexOutOfRange,
                        },
                    ),
                ),
            };
        }

        fn checkOperandStackTypes(self: *const @This(), expected: []const wasm.ValType) !void {
            if (expected.len == 0)
                return;

            if (expected.len > self.usableOperandStackDepth())
                return error.OperandStackUnderflow;

            for (self.operand_stack.items[self.operand_stack.items.len - expected.len ..], expected) |a, e|
                if (a.type != e)
                    return error.TypeMismatch;
        }

        fn usableOperandStackDepth(self: *const @This()) usize {
            return if (self.label_stack.getLastOrNull()) |l|
                self.label_stack.items.len - l.min_operand_stack_depth
            else
                self.label_stack.items.len;
        }
    };
}

const NullBackend = struct {
    pub fn visitLocals(_: *@This()) !struct {
        pub fn visitLocal(_: *@This(), _: wasm.ValType, _: wasm.LocalIdx, _: ?wasm.Name) !void {}

        pub fn finish(_: *@This()) !void {}

        pub fn deinit(_: *@This()) void {}
    } {
        return .{};
    }

    pub fn visitInstrs(_: *@This()) !struct {
        pub fn visitUnreachable(_: *@This()) !void {}

        pub fn finish(_: *@This()) !void {}

        pub fn deinit(_: *@This()) void {}
    } {
        return .{};
    }
};

test "ref all with null backend" {
    std.testing.refAllDecls(CodeProcessor(NullBackend));
}
