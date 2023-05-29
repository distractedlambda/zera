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

        fn getBackendType(comptime name: []const u8) type {
            return if (@hasDecl(Backend, name))
                @field(Backend, name)
            else
                void;
        }

        pub fn init(module_summary: *const ModuleSummary, allocator: std.mem.Allocator) @This() {
            return .{ .summary = module_summary, .allocator = allocator };
        }

        pub fn reset(self: *@This()) void {
            self.locals.clearRetainingCapacity();
            self.operand_stack.clearRetainingCapacity();
            self.label_stack.clearRetainingCapacity();
        }

        pub fn deinit(self: *@This()) void {
            self.locals.deinit(self.allocator);
            self.operand_stack.deinit(self.allocator);
            self.label_stack.deinit(self.allocator);
        }

        pub fn process(self: *@This(), backend: *Backend, function: wasm.FuncIdx, code: []const u8) !void {
            const func_type = self.summary.lookUpType(function);

            // Declare local variables for function parameters
            try self.beforeDeclaringLocals(backend, func_type.parameters.len);
            for (func_type.parameters) |t| try self.declareLocal(backend, function, t);

            var decoder = Decoder.init(code);

            // Declare other local variables
            for (0..try decoder.nextInt(u32)) |_| {
                const n_locals = try decoder.nextInt(u32);
                const local_type = try decoder.nextValType();
                try self.beforeDeclaringLocals(backend, n_locals);
                for (0..n_locals) |_| try self.declareLocal(backend, function, local_type);
            }

            if (@hasDecl(Backend, "doneDeclaringLocals")) {
                try backend.doneDeclaringLocals();
            }

            // Enter the implicit block surrounding the function body
            try self.enterBlock(backend, .{ .parameters = &.{}, .results = func_type.results });
        }

        fn beforeDeclaringLocals(self: *@This(), backend: *Backend, n_locals: usize) !void {
            try self.locals.ensureUnusedCapacity(self.allocator, n_locals);
            if (@hasDecl(Backend, "beforeDeclaringLocals"))
                try backend.beforeDeclaringLocals(n_locals);
        }

        fn declareLocal(self: *@This(), backend: *Backend, func: wasm.FuncIdx, typ: wasm.ValType) !void {
            const idx = wasm.LocalIdx{
                .value = std.math.cast(u32, self.locals.items.len) orelse
                    return error.TooManyLocals,
            };

            const name = self.summary.local_names.get(.{ func, idx });

            self.locals.appendAssumeCapacity(.{
                .type = typ,
                .backend = try backend.declareLocal(typ, idx, name),
            });
        }

        fn enterBlock(self: *@This(), backend: *Backend, typ: wasm.FuncType) !void {
            try self.label_stack.append(self.allocator, .{
                .type = typ,
                .backend = .{ .block = try backend.enterBlock(typ.results) },
            });
        }

        fn popLabel(self: *@This()) !Label {
            return self.label_stack.popOrNull() orelse return error.LabelStackUnderflow;
        }

        fn opEnd(self: *@This(), backend: *Backend) !void {
            return switch ((try self.popLabel()).backend) {
                .block => |b| backend.leaveBlock(b),
                .loop => |b| backend.leaveLoop(b),
                .@"if" => |b| backend.leaveIf(b),
                .@"else" => |b| backend.leaveElse(b),
            };
        }
    };
}

const NullBackend = struct {
    pub fn beforeDeclaringLocals(_: *@This(), _: usize) !void {}

    pub fn declareLocal(_: *@This(), _: wasm.ValType, _: wasm.LocalIdx, _: ?[]const u8) !void {}

    pub fn doneDeclaringLocals(_: *@This()) !void {}

    pub fn enterBlock(_: *@This(), _: []const wasm.ValType) !void {}
};

test "ref all with null backend" {
    std.testing.refAllDecls(CodeProcessor(NullBackend));
}
