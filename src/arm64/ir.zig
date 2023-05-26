const std = @import("std");
const wasm = @import("../wasm.zig");

pub const Instruction = struct {
    opcode: Opcode,

    fn init(opcode: Opcode) @This() {
        return .{ .opcode = opcode };
    }

    const Opcode = enum {
        trap,
        block_parameter,
        br,
        br_if,
        br_table,
        ret,
        call,
        call_indirect,
        funcref_null,
        externref_null,
        ref_func,
        global_get,
        global_set,
        table_get,
        table_set,
        table_size,
        memory_size,

        i32_const,
        i64_const,
        f32_const,
        f64_const,
        v128_const,

        i32_load,
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

        v128_load,
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
        v128_load32_zero,
        v128_load64_zero,

        v128_store,
    };

    const ControlDependent = struct {
        base: Instruction,
        prior: ?*Instruction = null,

        fn init(opcode: Opcode) @This() {
            return .{ .base = Instruction.init(opcode) };
        }
    };

    const Sequential = struct {
        base: ControlDependent,
        next: ?*Instruction = null,

        fn init(opcode: Opcode) @This() {
            return .{ .base = ControlDependent.init(opcode) };
        }
    };

    const Br = struct {
        base: Instruction = Instruction.init(.br),
        target: *Block,
        arguments: []*Instruction,
    };

    const BrIf = struct {
        base: Instruction = Instruction.init(.br_if),
        condition: *Instruction,
        target: *Block,
        arguments: []*Instruction,
    };

    const BrTable = struct {
        base: Instruction = Instruction.init(.br_table),
        index: *Instruction,
        cases: []*Instruction,
        default: *Instruction,
    };

    const Ret = struct {
        base: Instruction = Instruction.init(.ret),
        values: []*Instruction,
    };

    const Call = struct {
        base: Sequential = Sequential.init(.call),
        callee: wasm.FunctionIndex,
        arguments: []*Instruction,
    };

    const CallIndirect = struct {
        base: Sequential = Sequential.init(.call_indirect),
        callee_type: wasm.TypeIndex,
        table: wasm.TableIndex,
        index: *Instruction,
    };

    const RefFunc = struct {
        base: Instruction = Instruction.init(.ref_func),
        function: wasm.FunctionIndex,
    };

    const GlobalGet = struct {
        base: ControlDependent = ControlDependent.init(.global_get),
        global: wasm.GlobalIndex,
    };

    const GlobalSet = struct {
        base: Sequential = Sequential.init(.global_set),
        global: wasm.GlobalIndex,
        value: *Instruction,
    };

    const TableGet = struct {
        base: ControlDependent = ControlDependent.init(.table_get),
        table: wasm.TableIndex,
        index: *Instruction,
    };

    const TableSet = struct {
        base: Sequential = Sequential.init(.table_set),
        table: wasm.TableIndex,
        index: *Instruction,
        value: *Instruction,
    };

    const TableSize = struct {
        base: Instruction = Instruction.init(.table_size),
        table: wasm.TableIndex,
    };

    const Load = struct {
        base: ControlDependent,
        address: *Instruction,

        fn init(opcode: Opcode, address: *Instruction) @This() {
            return .{
                .base = ControlDependent.init(opcode),
                .address = address,
            };
        }
    };

    const Store = struct {
        base: Sequential,
        address: *Instruction,
        value: *Instruction,

        fn init(opcode: Opcode, address: *Instruction, value: *Instruction) @This() {
            return .{
                .base = Sequential.init(opcode),
                .address = address,
                .value = value,
            };
        }
    };

    const I32Const = struct {
        base: Instruction = Instruction.init(.i32_const),
        value: i32,
    };

    const I64Const = struct {
        base: Instruction = Instruction.init(.i64_const),
        value: i64,
    };

    const F32Const = struct {
        base: Instruction = Instruction.init(.f32_const),
        value: f32,
    };

    const F64Const = struct {
        base: Instruction = Instruction.init(.f64_const),
        value: f64,
    };

    const V128Const = struct {
        base: Instruction = Instruction.init(.v128_const),
        value: u128,
    };
};

pub const Block = struct {
    parameters: std.ArrayListUnmanaged(*Instruction.BlockParameter) = .{},
    start: ?*Instruction = null,
};
