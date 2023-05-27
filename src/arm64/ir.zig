const std = @import("std");
const wasm = @import("../wasm.zig");

pub const Node = struct {
    kind: Kind,

    pub fn init(kind: Kind) @This() {
        return .{ .kind = kind };
    }

    pub const Kind = enum {
        block,

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
};

pub const Block = struct {
    base: Node = Node.init(.block),
    start: ?*Node = null,

    pub fn init() @This() {
        return .{};
    }
};

pub const SequencedNode = struct {
    base: Node,
    prior: *Node,
    next: ?*Node = null,

    pub fn init(kind: Node.Kind, prior: *Node) @This() {
        return .{ .base = Node.init(kind), .prior = prior };
    }
};

pub const BrNode = struct {
    base: Node = Node.init(.br),
    target: *Block,
    arguments: []*Node,

    pub fn init(target: *Block, arguments: []*Node) @This() {
        return .{ .target = target, .arguments = arguments };
    }
};

pub const BrIfNode = struct {
    base: Node = Node.init(.br_if),
    condition: *Node,
    target: *Block,
    arguments: []*Node,

    pub fn init(condition: *Node, target: *Block, arguments: []*Node) @This() {
        return .{
            .condition = condition,
            .target = target,
            .arguments = arguments,
        };
    }
};

pub const BrTableNode = struct {
    base: Node = Node.init(.br_table),
    index: *Node,
    cases: []*Block,
    default: *Block,

    pub fn init(index: *Node, cases: []*Block, default: *Block) @This() {
        return .{
            .index = index,
            .cases = cases,
            .default = default,
        };
    }
};

pub const Ret = struct {
    base: Node = Node.init(.ret),
    values: []*Node,

    pub fn init(values: []*Node) @This() {
        return .{ .values = values };
    }
};

pub const Call = struct {
    base: SequencedNode,
    callee: wasm.FunctionIndex,
    arguments: []*Node,

    pub fn init(prior: *Node, callee: wasm.FunctionIndex, arguments: []*Node) @This() {
        return .{
            .base = SequencedNode.init(.call, prior),
            .callee = callee,
            .arguments = arguments,
        };
    }
};

pub const CallIndirect = struct {
    base: SequencedNode,
    callee_type: wasm.TypeIndex,
    table: wasm.TableIndex,
    index: *Node,
    arguments: []*Node,

    pub fn init(prior: *Node, callee_type: wasm.TypeIndex, table: wasm.TableIndex, index: *Node, arguments: []*Node) @This() {
        return .{
            .base = SequencedNode.init(.call_indirect, prior),
            .callee_type = callee_type,
            .table = table,
            .index = index,
            .arguments = arguments,
        };
    }
};

pub const RefFunc = struct {
    base: Node = Node.init(.ref_func),
    function: wasm.FunctionIndex,

    pub fn init(function: wasm.FunctionIndex) @This() {
        return .{ .function = function };
    }
};

pub const GlobalGet = struct {
    base: SequencedNode,
    global: wasm.GlobalIndex,

    pub fn init(prior: *Node, global: wasm.GlobalIndex) @This() {
        return .{
            .base = SequencedNode.init(.global_get, prior),
            .global = global,
        };
    }
};

pub const GlobalSet = struct {
    base: SequencedNode,
    global: wasm.GlobalIndex,
    value: *Node,

    pub fn init(prior: *Node, global: wasm.GlobalIndex, value: *Node) @This() {
        return .{
            .base = SequencedNode.init(.global_set, prior),
            .global = global,
            .value = value,
        };
    }
};

pub const TableGet = struct {
    base: SequencedNode,
    table: wasm.TableIndex,
    index: *Node,
};

pub const TableSet = struct {
    base: SequencedNode,
    table: wasm.TableIndex,
    index: *Node,
    value: *Node,
};

pub const TableSize = struct {
    base: SequencedNode,
    table: wasm.TableIndex,
};

pub const Load = struct {
    base: SequencedNode,
    address: *Node,
};

pub const Store = struct {
    base: SequencedNode,
    address: *Node,
    value: *Node,
};

pub const I32Const = struct {
    base: Node = Node.init(.i32_const),
    value: i32,

    pub fn init(value: i32) @This() {
        return .{ .value = value };
    }
};

pub const I64Const = struct {
    base: Node = Node.init(.i64_const),
    value: i64,

    pub fn init(value: i64) @This() {
        return .{ .value = value };
    }
};

pub const F32Const = struct {
    base: Node = Node.init(.f32_const),
    value: f32,

    pub fn init(value: f32) @This() {
        return .{ .value = value };
    }
};

pub const F64Const = struct {
    base: Node = Node.init(.f64_const),
    value: f64,

    pub fn init(value: f64) @This() {
        return .{ .value = value };
    }
};

pub const V128Const = struct {
    base: Node = Node.init(.v128_const),
    value: u128,

    pub fn init(value: u128) @This() {
        return .{ .value = value };
    }
};
