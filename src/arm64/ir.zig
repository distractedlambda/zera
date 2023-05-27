const std = @import("std");
const wasm = @import("../wasm.zig");

pub const Opcode = enum {
    trap,
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

pub const Instruction = struct {
    opcode: Opcode,

    pub fn init(opcode: Opcode) @This() {
        return .{ .opcode = opcode };
    }
};

pub const Block = struct {
    head: ?*Instruction = null,
    tail: ?*Instruction = null,
};

pub const SequencedInstruction = struct {
    base: Instruction,
    prior: ?*Instruction = null,
    next: ?*Instruction = null,

    pub fn init(opcode: Opcode) @This() {
        return .{ .base = Instruction.init(opcode) };
    }
};

pub const Br = struct {
    base: Instruction = Instruction.init(.br),
    target: *Block,
    arguments: []*Instruction,
};

pub const BrIf = struct {
    base: Instruction = Instruction.init(.br_if),
    condition: *Instruction,
    target: *Block,
    arguments: []*Instruction,
};

pub const BrTable = struct {
    base: Instruction = Instruction.init(.br_table),
    index: *Instruction,
    cases: []*Block,
    default: *Block,
};

pub const Ret = struct {
    base: Instruction = Instruction.init(.ret),
    values: []*Instruction,
};

pub const Call = struct {
    base: SequencedInstruction = SequencedInstruction.init(.call),
    callee: wasm.FunctionIndex,
    arguments: []*Instruction,
};

pub const CallIndirect = struct {
    base: SequencedInstruction = SequencedInstruction.init(.call_indirect),
    callee_type: wasm.TypeIndex,
    table: wasm.TableIndex,
    index: *Instruction,
    arguments: []*Instruction,
};

pub const RefFunc = struct {
    base: Instruction = Instruction.init(.ref_func),
    function: wasm.FunctionIndex,
};

pub const GlobalGet = struct {
    base: SequencedInstruction = SequencedInstruction.init(.global_get),
    global: wasm.GlobalIndex,
};

pub const GlobalSet = struct {
    base: SequencedInstruction = SequencedInstruction.init(.global_set),
    global: wasm.GlobalIndex,
    value: *Instruction,
};

pub const TableGet = struct {
    base: SequencedInstruction = SequencedInstruction.init(.table_get),
    table: wasm.TableIndex,
    index: *Instruction,
};

pub const TableSet = struct {
    base: SequencedInstruction = SequencedInstruction.init(.table_set),
    table: wasm.TableIndex,
    index: *Instruction,
    value: *Instruction,
};

pub const TableSize = struct {
    base: SequencedInstruction = SequencedInstruction.init(.table_size),
    table: wasm.TableIndex,
};

pub const Load = struct {
    base: SequencedInstruction,
    address: *Instruction,

    pub fn init(opcode: Opcode, address: *Instruction) @This() {
        return .{
            .base = SequencedInstruction.init(opcode),
            .address = address,
        };
    }
};

pub const Store = struct {
    base: SequencedInstruction,
    address: *Instruction,
    value: *Instruction,

    pub fn init(opcode: Opcode, address: *Instruction, value: *Instruction) @This() {
        return .{
            .base = SequencedInstruction.init(opcode),
            .address = address,
            .value = value,
        };
    }
};

pub const I32Const = struct {
    base: Instruction = Instruction.init(.i32_const),
    value: i32,
};

pub const I64Const = struct {
    base: Instruction = Instruction.init(.i64_const),
    value: i64,
};

pub const F32Const = struct {
    base: Instruction = Instruction.init(.f32_const),
    value: f32,
};

pub const F64Const = struct {
    base: Instruction = Instruction.init(.f64_const),
    value: f64,
};

pub const V128Const = struct {
    base: Instruction = Instruction.init(.v128_const),
    value: u128,
};
