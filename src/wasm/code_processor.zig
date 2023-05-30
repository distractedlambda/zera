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
        outermost_rest_unreachable: bool = false,

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

            backend: union(enum) {
                block: BackendBlock,
                loop: BackendLoop,
                @"if": BackendIf,
                @"else": BackendElse,
            },

            rest_unreachable: bool = false,
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
                opcodes.@"unreachable" => if (!self.restUnreachable()) {
                    try visitor.visitUnreachable();
                    self.markRestUnreachable();
                },

                opcodes.nop => {},

                else => return error.UnsupportedOpcode,
            };

            if (!decoder.atEnd())
                return error.CodeAfterEnd;

            if (!self.outermost_rest_unreachable)
                @panic("TODO handle implicit trailing return");
        }

        fn restUnreachable(self: *const @This()) bool {
            return if (self.label_stack.getLastOrNull()) |l|
                l.rest_unreachable
            else
                self.outermost_rest_unreachable;
        }

        fn markRestUnreachable(self: *@This()) void {
            if (self.label_stack.items.len != 0) {
                self.label_stack.items[self.label_stack.items.len - 1].rest_unreachable = true;
            } else {
                self.outermost_rest_unreachable = true;
            }
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
