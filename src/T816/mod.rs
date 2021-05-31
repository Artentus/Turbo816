pub mod compile;
pub mod grammar;

use crate::*;
use std::fmt::{Debug, Display};

const ARGUMENT_REGISTER_COUNT: usize = 16;
const RESULT_REGISTER_COUNT: usize = 10;
const ERROR_REGISTER_COUNT: usize = 2;
const GLOBAL_REGISTER_COUNT: usize = 100;

const ZERO: Expression = Expression::Literal(Wrapping(0));
const MAX_VAL: Expression = Expression::Literal(Wrapping(0xFFFF));
const VECTOR_OFFSET: Expression = Expression::Literal(Wrapping(0xFFE0));
const COMPILER_FUNCTIONS_OFFSET: Expression = Expression::Literal(Wrapping(0xFF00));

#[allow(dead_code)]
const N_FLAG: Expression = Expression::Literal(Wrapping(0x80));
#[allow(dead_code)]
const O_FLAG: Expression = Expression::Literal(Wrapping(0x40));
#[allow(dead_code)]
const M_FLAG: Expression = Expression::Literal(Wrapping(0x20));
#[allow(dead_code)]
const X_FLAG: Expression = Expression::Literal(Wrapping(0x10));
#[allow(dead_code)]
const D_FLAG: Expression = Expression::Literal(Wrapping(0x08));
#[allow(dead_code)]
const I_FLAG: Expression = Expression::Literal(Wrapping(0x04));
#[allow(dead_code)]
const Z_FLAG: Expression = Expression::Literal(Wrapping(0x02));
#[allow(dead_code)]
const C_FLAG: Expression = Expression::Literal(Wrapping(0x01));

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IndexRegister {
    X,
    Y,
}
impl Display for IndexRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexRegister::X => {
                write!(f, "X")
            }
            IndexRegister::Y => {
                write!(f, "Y")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GeneralPurposeRegister {
    Argument(usize),
    Result(usize),
    Error(usize),
    Global(usize),
    Identifier(String),
}
impl Display for GeneralPurposeRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GeneralPurposeRegister::Argument(i) => {
                write!(f, "ARG{}", i)
            }
            GeneralPurposeRegister::Result(i) => {
                write!(f, "RET{}", i)
            }
            GeneralPurposeRegister::Error(i) => {
                write!(f, "ERR{}", i)
            }
            GeneralPurposeRegister::Global(i) => {
                write!(f, "R{}", i)
            }
            GeneralPurposeRegister::Identifier(name) => {
                write!(f, "{}", name)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SpecialPurposeRegister {
    StackPointer,
    StatusFlags,
    DataBank,
}
impl Display for SpecialPurposeRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpecialPurposeRegister::StackPointer => {
                write!(f, "SP")
            }
            SpecialPurposeRegister::StatusFlags => {
                write!(f, "FL")
            }
            SpecialPurposeRegister::DataBank => {
                write!(f, "DB")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TransferArg {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
    SpecialPurposeRegister(SpecialPurposeRegister),
}
impl Display for TransferArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransferArg::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            TransferArg::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            TransferArg::SpecialPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LoadWordSource {
    Immediate(Expression),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for LoadWordSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadWordSource::Immediate(value) => {
                write!(f, "#{}", value.to_hex(2, true))
            }
            LoadWordSource::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            LoadWordSource::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            LoadWordSource::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            LoadWordSource::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LoadByteSource {
    Immediate(Expression),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for LoadByteSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadByteSource::Immediate(value) => {
                write!(f, "#{}", value.to_hex(1, true))
            }
            LoadByteSource::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            LoadByteSource::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            LoadByteSource::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            LoadByteSource::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LoadTarget {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
}
impl Display for LoadTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadTarget::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            LoadTarget::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StoreSource {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
    Immediate(Expression),
}
impl Display for StoreSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoreSource::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            StoreSource::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            StoreSource::Immediate(value) => {
                write!(f, "#{}", value.to_hex(2, true))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StoreTarget {
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for StoreTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoreTarget::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            StoreTarget::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            StoreTarget::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            StoreTarget::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StackArg {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
    SpecialPurposeRegister(SpecialPurposeRegister),
}
impl Display for StackArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackArg::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            StackArg::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            StackArg::SpecialPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CountTarget {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
}
impl Display for CountTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CountTarget::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            CountTarget::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            CountTarget::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            CountTarget::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AluSource {
    GeneralPurposeRegister(GeneralPurposeRegister),
    Immediate(Expression),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for AluSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AluSource::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            AluSource::Immediate(value) => {
                write!(f, "#{}", value.to_hex(2, true))
            }
            AluSource::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            AluSource::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            AluSource::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            AluSource::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AluTarget {
    GeneralPurposeRegister(GeneralPurposeRegister),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for AluTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AluTarget::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            AluTarget::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            AluTarget::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            AluTarget::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            AluTarget::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum JumpTarget {
    Absolute(Expression),
    AbsoluteIndirect(Expression),
    AbsoluteIndexedIndirect(Expression, IndexRegister),
}
impl Display for JumpTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpTarget::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(3, true))
            }
            JumpTarget::AbsoluteIndirect(addr) => {
                write!(f, "[{}]", addr.to_hex(2, true))
            }
            JumpTarget::AbsoluteIndexedIndirect(addr, index) => {
                write!(f, "[{}:{}]", addr.to_hex(2, true), index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompareSource {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
}
impl Display for CompareSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompareSource::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            CompareSource::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompareWithSource {
    GeneralPurposeRegister(GeneralPurposeRegister),
    Immediate(Expression),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for CompareWithSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompareWithSource::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            CompareWithSource::Immediate(value) => {
                write!(f, "#{}", value.to_hex(2, true))
            }
            CompareWithSource::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            CompareWithSource::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            CompareWithSource::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            CompareWithSource::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum StatusFlag {
    N,
    V,
    D,
    I,
    Z,
    C,
}
impl Display for StatusFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatusFlag::N => write!(f, "N"),
            StatusFlag::V => write!(f, "V"),
            StatusFlag::D => write!(f, "D"),
            StatusFlag::I => write!(f, "I"),
            StatusFlag::Z => write!(f, "Z"),
            StatusFlag::C => write!(f, "C"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StatusFlagsSource(DisplayableVec<StatusFlag>);
impl StatusFlagsSource {
    pub fn get_value(&self) -> u8 {
        let mut result: u8 = 0x00;
        for f in self.0 .0.iter() {
            result |= match f {
                StatusFlag::N => 0x80,
                StatusFlag::V => 0x40,
                StatusFlag::D => 0x08,
                StatusFlag::I => 0x04,
                StatusFlag::Z => 0x02,
                StatusFlag::C => 0x01,
            }
        }
        result
    }
}
impl Display for StatusFlagsSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BitTestSource {
    GeneralPurposeRegister(GeneralPurposeRegister),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
}
impl Display for BitTestSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BitTestSource::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            BitTestSource::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            BitTestSource::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BitTestWithSource {
    GeneralPurposeRegister(GeneralPurposeRegister),
    Immediate(Expression),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for BitTestWithSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BitTestWithSource::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            BitTestWithSource::Immediate(value) => {
                write!(f, "#{}", value.to_hex(2, true))
            }
            BitTestWithSource::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            BitTestWithSource::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            BitTestWithSource::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            BitTestWithSource::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Transfer(TransferArg, TransferArg),
    LoadWord(LoadTarget, LoadWordSource),
    StoreWord(StoreSource, StoreTarget),
    LoadByte(LoadTarget, LoadByteSource),
    StoreByte(StoreSource, StoreTarget),

    Push(StackArg),
    Pop(StackArg),

    Increment(CountTarget),
    Decrement(CountTarget),

    Add(AluSource, AluSource, AluTarget),
    AddWithCarry(AluSource, AluSource, AluTarget),
    Subtract(AluSource, AluSource, AluTarget),
    SubtractWithBorrow(AluSource, AluSource, AluTarget),
    BitwiseAnd(AluSource, AluSource, AluTarget),
    BitwiseXor(AluSource, AluSource, AluTarget),
    BitwiseOr(AluSource, AluSource, AluTarget),

    Negate(AluSource, AluTarget),
    BitwiseNot(AluSource, AluTarget),
    ShiftLeft(AluSource, AluTarget),
    ShiftRight(AluSource, AluTarget),
    RotateLeft(AluSource, AluTarget),
    RotateRight(AluSource, AluTarget),

    JumpIfEqual(JumpTarget),
    JumpIfNotEqual(JumpTarget),
    JumpIfGreater(JumpTarget),
    JumpIfLess(JumpTarget),
    JumpIfGreaterOrEqual(JumpTarget),
    JumpIfLessOrEqual(JumpTarget),
    JumpOnCarry(JumpTarget),
    JumpOnCarryClear(JumpTarget),
    JumpOnOverflow(JumpTarget),
    JumpOnNoOverflow(JumpTarget),
    Jump(JumpTarget),

    Compare(CompareSource, CompareWithSource),

    SetStatusFlags(StatusFlagsSource),
    ResetStatusFlags(StatusFlagsSource),

    BitTest(BitTestSource, BitTestWithSource),

    SoftwareInterrupt,
    WaitForInterrupt,
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Transfer(s, t) => {
                write!(f, "TR {} -> {}", s, t)
            }
            Instruction::LoadWord(t, s) => {
                write!(f, "LDW {} <- {}", t, s)
            }
            Instruction::StoreWord(s, t) => {
                write!(f, "STW {} -> {}", s, t)
            }
            Instruction::LoadByte(t, s) => {
                write!(f, "LDB {} <- {}", t, s)
            }
            Instruction::StoreByte(s, t) => {
                write!(f, "STB {} -> {}", s, t)
            }
            Instruction::Push(s) => {
                write!(f, "PUSH {}", s)
            }
            Instruction::Pop(t) => {
                write!(f, "POP {}", t)
            }
            Instruction::Increment(t) => {
                write!(f, "INC {}", t)
            }
            Instruction::Decrement(t) => {
                write!(f, "DEC {}", t)
            }
            Instruction::Add(l, r, t) => {
                write!(f, "ADD {}, {} -> {}", l, r, t)
            }
            Instruction::AddWithCarry(l, r, t) => {
                write!(f, "ADDC {}, {} -> {}", l, r, t)
            }
            Instruction::Subtract(l, r, t) => {
                write!(f, "SUB {}, {} -> {}", l, r, t)
            }
            Instruction::SubtractWithBorrow(l, r, t) => {
                write!(f, "SUBB {}, {} -> {}", l, r, t)
            }
            Instruction::BitwiseAnd(l, r, t) => {
                write!(f, "AND {}, {} -> {}", l, r, t)
            }
            Instruction::BitwiseXor(l, r, t) => {
                write!(f, "XOR {}, {} -> {}", l, r, t)
            }
            Instruction::BitwiseOr(l, r, t) => {
                write!(f, "OR {}, {} -> {}", l, r, t)
            }
            Instruction::Negate(s, t) => {
                write!(f, "NEG {} -> {}", s, t)
            }
            Instruction::BitwiseNot(s, t) => {
                write!(f, "NOT {} -> {}", s, t)
            }
            Instruction::ShiftLeft(s, t) => {
                write!(f, "SHL {} -> {}", s, t)
            }
            Instruction::ShiftRight(s, t) => {
                write!(f, "SHR {} -> {}", s, t)
            }
            Instruction::RotateLeft(s, t) => {
                write!(f, "ROL {} -> {}", s, t)
            }
            Instruction::RotateRight(s, t) => {
                write!(f, "ROR {} -> {}", s, t)
            }
            Instruction::JumpIfEqual(t) => {
                write!(f, "JPEQ {}", t)
            }
            Instruction::JumpIfNotEqual(t) => {
                write!(f, "JPNEQ {}", t)
            }
            Instruction::JumpIfGreater(t) => {
                write!(f, "JPG {}", t)
            }
            Instruction::JumpIfLess(t) => {
                write!(f, "JPL {}", t)
            }
            Instruction::JumpIfGreaterOrEqual(t) => {
                write!(f, "JPGEQ {}", t)
            }
            Instruction::JumpIfLessOrEqual(t) => {
                write!(f, "JPLEQ {}", t)
            }
            Instruction::JumpOnCarry(t) => {
                write!(f, "JPC {}", t)
            }
            Instruction::JumpOnCarryClear(t) => {
                write!(f, "JPCC {}", t)
            }
            Instruction::JumpOnOverflow(t) => {
                write!(f, "JPO {}", t)
            }
            Instruction::JumpOnNoOverflow(t) => {
                write!(f, "JPNO {}", t)
            }
            Instruction::Jump(t) => {
                write!(f, "JP {}", t)
            }
            Instruction::Compare(l, r) => {
                write!(f, "CMP {}, {}", l, r)
            }
            Instruction::SetStatusFlags(s) => {
                write!(f, "SSF {}", s)
            }
            Instruction::ResetStatusFlags(s) => {
                write!(f, "RSF {}", s)
            }
            Instruction::BitTest(l, r) => {
                write!(f, "BIT {}, {}", l, r)
            }
            Instruction::SoftwareInterrupt => {
                write!(f, "BRK")
            }
            Instruction::WaitForInterrupt => {
                write!(f, "WAITI")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Parameter {
    IndexRegister(IndexRegister),
    GeneralPurposeRegister(GeneralPurposeRegister),
    Immediate(Expression),
    Absolute(Expression),
    AbsoluteIndexed(Expression, IndexRegister),
    RegisterIndirect(GeneralPurposeRegister),
    RegisterIndirectIndexed(GeneralPurposeRegister, IndexRegister),
}
impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Parameter::IndexRegister(reg) => {
                write!(f, "{}", reg)
            }
            Parameter::GeneralPurposeRegister(reg) => {
                write!(f, "{}", reg)
            }
            Parameter::Immediate(value) => {
                write!(f, "#{}", value.to_hex(2, true))
            }
            Parameter::Absolute(addr) => {
                write!(f, "{}", addr.to_hex(2, true))
            }
            Parameter::AbsoluteIndexed(addr, index) => {
                write!(f, "{}:{}", addr.to_hex(2, true), index)
            }
            Parameter::RegisterIndirect(reg) => {
                write!(f, "[{}]", reg)
            }
            Parameter::RegisterIndirectIndexed(reg, index) => {
                write!(f, "[{}]:{}", reg, index)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Directive {
    Origin(Expression),
    Define(String, Expression),
    DefineRegister(String, GeneralPurposeRegister),
    StoreByte(DisplayableVec<Expression>),
    StoreWord(DisplayableVec<Expression>),
    AsciiString(String, bool),
    UnicodeString(String, bool),
    Call(String, DisplayableVec<Parameter>),
    Return,
}
impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Origin(expr) => {
                write!(f, ".org {}", expr.to_hex(3, true))
            }
            Directive::Define(name, expr) => {
                write!(f, ".def {} = {:X}", name, expr)
            }
            Directive::DefineRegister(name, reg) => {
                write!(f, ".reg {} = {}", name, reg)
            }
            Directive::StoreByte(exprs) => {
                write!(f, ".byte {}", exprs.to_hex(1, true))
            }
            Directive::StoreWord(exprs) => {
                write!(f, ".word {}", exprs.to_hex(2, true))
            }
            Directive::AsciiString(s, null_terminated) => {
                if *null_terminated {
                    write!(f, ".asciiz \"{}\"", s)
                } else {
                    write!(f, ".ascii \"{}\"", s)
                }
            }
            Directive::UnicodeString(s, null_terminated) => {
                if *null_terminated {
                    write!(f, ".unicodez \"{}\"", s)
                } else {
                    write!(f, ".unicode \"{}\"", s)
                }
            }
            Directive::Call(name, params) => {
                write!(f, ".call {}({})", name, params)
            }
            Directive::Return => {
                write!(f, ".return")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Instruction(Instruction),
    Directive(Directive),
    Label(String),
    Empty,
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Instruction(inst) => {
                write!(f, "{}", inst)
            }
            Statement::Directive(dir) => {
                write!(f, "{}", dir)
            }
            Statement::Label(l) => {
                write!(f, "@{}:", l)
            }
            Statement::Empty => {
                write!(f, "")
            }
        }
    }
}

#[derive(Debug)]
pub enum TopLevelStatement {
    Statement(Statement),
    Function(String, DisplayableVec<String>, Vec<Statement>),
    MainEntryPoint(Vec<Statement>),
    IrqEntryPoint(Vec<Statement>),
    NmiEntryPoint(Vec<Statement>),
    BrkEntryPoint(Vec<Statement>),
}
impl Display for TopLevelStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_entry_point {
            ($body:expr, $open:literal, $close:literal) => {
                match writeln!(f, $open) {
                    Ok(_) => {
                        for stm in $body.iter() {
                            writeln!(f, "\t{}", stm)?;
                        }
                        write!(f, $close)
                    }
                    Err(err) => Err(err),
                }
            };
        }

        match self {
            TopLevelStatement::Statement(stm) => {
                write!(f, "{}", stm)
            }
            TopLevelStatement::Function(name, params, body) => {
                writeln!(f, ".fn {}({})", name, params)?;
                for stm in body.iter() {
                    writeln!(f, "\t{}", stm)?;
                }
                write!(f, ".endfn")
            }
            TopLevelStatement::MainEntryPoint(body) => {
                write_entry_point!(body, ".main", ".endmain")
            }
            TopLevelStatement::IrqEntryPoint(body) => {
                write_entry_point!(body, ".irq", ".endirq")
            }
            TopLevelStatement::NmiEntryPoint(body) => {
                write_entry_point!(body, ".nmi", ".endnmi")
            }
            TopLevelStatement::BrkEntryPoint(body) => {
                write_entry_point!(body, ".brk", ".endbrk")
            }
        }
    }
}

#[derive(Debug)]
pub struct Program {
    body: Vec<TopLevelStatement>,
}
impl Program {
    fn new() -> Self {
        Self { body: Vec::new() }
    }
}
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stm in self.body.iter() {
            writeln!(f, "{}", stm)?;
        }
        Ok(())
    }
}
