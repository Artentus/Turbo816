use super::*;
use crate::*;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::Error;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use std::fmt::Debug;
use std::fs::File;
use std::path::{Path, PathBuf};

fn comma_sep(input: &str) -> IResult<&str, (), Error<&str>> {
    discard(delimited(multispace0, char(','), multispace0))(input)
}
fn colon_sep(input: &str) -> IResult<&str, (), Error<&str>> {
    discard(delimited(multispace0, char(':'), multispace0))(input)
}
fn equals_sep(input: &str) -> IResult<&str, (), Error<&str>> {
    discard(delimited(multispace0, char('='), multispace0))(input)
}
fn left_arrow_sep(input: &str) -> IResult<&str, (), Error<&str>> {
    discard(delimited(multispace0, tag("<-"), multispace0))(input)
}
fn right_arrow_sep(input: &str) -> IResult<&str, (), Error<&str>> {
    discard(delimited(multispace0, tag("->"), multispace0))(input)
}

fn comment(input: &str) -> IResult<&str, &str, Error<&str>> {
    preceded(tag("//"), take_while(|_| true))(input)
}

fn index_register(input: &str) -> IResult<&str, IndexRegister, Error<&str>> {
    alt((
        then_return(one_of("xX"), IndexRegister::X),
        then_return(one_of("yY"), IndexRegister::Y),
    ))(input)
}

fn general_purpose_register(input: &str) -> IResult<&str, GeneralPurposeRegister, Error<&str>> {
    alt((
        map(
            verify(preceded(tag_no_case("ARG"), decimal_number), |i| {
                *i < ARGUMENT_REGISTER_COUNT as isize
            }),
            |i| GeneralPurposeRegister::Argument(i as usize),
        ),
        map(
            verify(preceded(tag_no_case("RET"), decimal_number), |i| {
                *i < RESULT_REGISTER_COUNT as isize
            }),
            |i| GeneralPurposeRegister::Result(i as usize),
        ),
        map(
            verify(preceded(tag_no_case("ERR"), decimal_number), |i| {
                *i < ERROR_REGISTER_COUNT as isize
            }),
            |i| GeneralPurposeRegister::Error(i as usize),
        ),
        map(
            verify(preceded(one_of("rR"), decimal_number), |i| {
                *i < GLOBAL_REGISTER_COUNT as isize
            }),
            |i| GeneralPurposeRegister::Global(i as usize),
        ),
        map(preceded(char('$'), identifier), |name| {
            GeneralPurposeRegister::Identifier(name.to_string())
        }),
    ))(input)
}

fn special_purpose_register(input: &str) -> IResult<&str, SpecialPurposeRegister, Error<&str>> {
    alt((
        then_return(tag_no_case("SP"), SpecialPurposeRegister::StackPointer),
        then_return(tag_no_case("FL"), SpecialPurposeRegister::StatusFlags),
        then_return(tag_no_case("DB"), SpecialPurposeRegister::DataBank),
    ))(input)
}

fn immediate(input: &str) -> IResult<&str, Expression, Error<&str>> {
    preceded(char('#'), expression)(input)
}

fn absolute(input: &str) -> IResult<&str, Expression, Error<&str>> {
    expression(input)
}

fn absolute_indexed(input: &str) -> IResult<&str, (Expression, IndexRegister), Error<&str>> {
    map(
        tuple((expression, colon_sep, index_register)),
        |(a, _, i)| (a, i),
    )(input)
}

fn register_indirect(input: &str) -> IResult<&str, GeneralPurposeRegister, Error<&str>> {
    delimited(
        char('['),
        delimited(multispace0, general_purpose_register, multispace0),
        char(']'),
    )(input)
}

fn register_indirect_indexed(
    input: &str,
) -> IResult<&str, (GeneralPurposeRegister, IndexRegister), Error<&str>> {
    map(
        tuple((
            delimited(
                char('['),
                delimited(multispace0, general_purpose_register, multispace0),
                char(']'),
            ),
            colon_sep,
            index_register,
        )),
        |(r, _, i)| (r, i),
    )(input)
}

fn absolute_indirect(input: &str) -> IResult<&str, Expression, Error<&str>> {
    delimited(
        char('['),
        delimited(multispace0, expression, multispace0),
        char(']'),
    )(input)
}

fn absolute_indexed_indirect(
    input: &str,
) -> IResult<&str, (Expression, IndexRegister), Error<&str>> {
    map(
        delimited(
            char(']'),
            tuple((
                delimited(multispace0, expression, multispace0),
                colon_sep,
                index_register,
            )),
            char(']'),
        ),
        |(a, _, i)| (a, i),
    )(input)
}

fn transfer_arg(input: &str) -> IResult<&str, TransferArg, Error<&str>> {
    alt((
        map(index_register, |r| TransferArg::IndexRegister(r)),
        map(general_purpose_register, |r| {
            TransferArg::GeneralPurposeRegister(r)
        }),
        map(special_purpose_register, |r| {
            TransferArg::SpecialPurposeRegister(r)
        }),
    ))(input)
}

fn load_word_source(input: &str) -> IResult<&str, LoadWordSource, Error<&str>> {
    alt((
        map(immediate, |v| LoadWordSource::Immediate(v)),
        map(absolute_indexed, |(a, i)| {
            LoadWordSource::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| LoadWordSource::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            LoadWordSource::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| LoadWordSource::RegisterIndirect(r)),
    ))(input)
}

fn load_byte_source(input: &str) -> IResult<&str, LoadByteSource, Error<&str>> {
    alt((
        map(immediate, |v| LoadByteSource::Immediate(v)),
        map(absolute_indexed, |(a, i)| {
            LoadByteSource::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| LoadByteSource::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            LoadByteSource::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| LoadByteSource::RegisterIndirect(r)),
    ))(input)
}

fn load_target(input: &str) -> IResult<&str, LoadTarget, Error<&str>> {
    alt((
        map(index_register, |r| LoadTarget::IndexRegister(r)),
        map(general_purpose_register, |r| {
            LoadTarget::GeneralPurposeRegister(r)
        }),
    ))(input)
}

fn store_source(input: &str) -> IResult<&str, StoreSource, Error<&str>> {
    alt((
        map(index_register, |r| StoreSource::IndexRegister(r)),
        map(general_purpose_register, |r| {
            StoreSource::GeneralPurposeRegister(r)
        }),
        map(immediate, |v| StoreSource::Immediate(v)),
    ))(input)
}

fn store_target(input: &str) -> IResult<&str, StoreTarget, Error<&str>> {
    alt((
        map(absolute_indexed, |(a, i)| {
            StoreTarget::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| StoreTarget::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            StoreTarget::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| StoreTarget::RegisterIndirect(r)),
    ))(input)
}

fn stack_arg(input: &str) -> IResult<&str, StackArg, Error<&str>> {
    alt((
        map(index_register, |r| StackArg::IndexRegister(r)),
        map(general_purpose_register, |r| {
            StackArg::GeneralPurposeRegister(r)
        }),
        map(special_purpose_register, |r| {
            StackArg::SpecialPurposeRegister(r)
        }),
    ))(input)
}

fn count_target(input: &str) -> IResult<&str, CountTarget, Error<&str>> {
    alt((
        map(index_register, |r| CountTarget::IndexRegister(r)),
        map(general_purpose_register, |r| {
            CountTarget::GeneralPurposeRegister(r)
        }),
        map(absolute_indexed, |(a, i)| {
            CountTarget::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| CountTarget::Absolute(a)),
    ))(input)
}

fn alu_source(input: &str) -> IResult<&str, AluSource, Error<&str>> {
    alt((
        map(general_purpose_register, |r| {
            AluSource::GeneralPurposeRegister(r)
        }),
        map(immediate, |v| AluSource::Immediate(v)),
        map(absolute_indexed, |(a, i)| AluSource::AbsoluteIndexed(a, i)),
        map(absolute, |a| AluSource::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            AluSource::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| AluSource::RegisterIndirect(r)),
    ))(input)
}

fn alu_target(input: &str) -> IResult<&str, AluTarget, Error<&str>> {
    alt((
        map(general_purpose_register, |r| {
            AluTarget::GeneralPurposeRegister(r)
        }),
        map(absolute_indexed, |(a, i)| AluTarget::AbsoluteIndexed(a, i)),
        map(absolute, |a| AluTarget::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            AluTarget::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| AluTarget::RegisterIndirect(r)),
    ))(input)
}

fn jump_target(input: &str) -> IResult<&str, JumpTarget, Error<&str>> {
    alt((
        map(absolute, |a| JumpTarget::Absolute(a)),
        map(absolute_indexed_indirect, |(a, i)| {
            JumpTarget::AbsoluteIndexedIndirect(a, i)
        }),
        map(absolute_indirect, |a| JumpTarget::AbsoluteIndirect(a)),
    ))(input)
}

fn compare_source(input: &str) -> IResult<&str, CompareSource, Error<&str>> {
    alt((
        map(index_register, |r| CompareSource::IndexRegister(r)),
        map(general_purpose_register, |r| {
            CompareSource::GeneralPurposeRegister(r)
        }),
    ))(input)
}

fn compare_with_source(input: &str) -> IResult<&str, CompareWithSource, Error<&str>> {
    alt((
        map(general_purpose_register, |r| {
            CompareWithSource::GeneralPurposeRegister(r)
        }),
        map(immediate, |v| CompareWithSource::Immediate(v)),
        map(absolute_indexed, |(a, i)| {
            CompareWithSource::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| CompareWithSource::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            CompareWithSource::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| {
            CompareWithSource::RegisterIndirect(r)
        }),
    ))(input)
}

fn status_flags_source(input: &str) -> IResult<&str, StatusFlagsSource, Error<&str>> {
    fn status_flag(input: &str) -> IResult<&str, StatusFlag, Error<&str>> {
        alt((
            then_return(alt((char('N'), char('n'))), StatusFlag::N),
            then_return(alt((char('V'), char('v'))), StatusFlag::V),
            then_return(alt((char('D'), char('d'))), StatusFlag::D),
            then_return(alt((char('I'), char('i'))), StatusFlag::I),
            then_return(alt((char('Z'), char('z'))), StatusFlag::Z),
            then_return(alt((char('C'), char('c'))), StatusFlag::C),
        ))(input)
    }

    map(separated_list1(comma_sep, status_flag), |flags| {
        StatusFlagsSource(DisplayableVec(flags))
    })(input)
}

fn bit_test_source(input: &str) -> IResult<&str, BitTestSource, Error<&str>> {
    alt((
        map(general_purpose_register, |r| {
            BitTestSource::GeneralPurposeRegister(r)
        }),
        map(absolute_indexed, |(a, i)| {
            BitTestSource::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| BitTestSource::Absolute(a)),
    ))(input)
}

fn bit_test_with_source(input: &str) -> IResult<&str, BitTestWithSource, Error<&str>> {
    alt((
        map(general_purpose_register, |r| {
            BitTestWithSource::GeneralPurposeRegister(r)
        }),
        map(immediate, |v| BitTestWithSource::Immediate(v)),
        map(absolute_indexed, |(a, i)| {
            BitTestWithSource::AbsoluteIndexed(a, i)
        }),
        map(absolute, |a| BitTestWithSource::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            BitTestWithSource::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| {
            BitTestWithSource::RegisterIndirect(r)
        }),
    ))(input)
}

fn instruction(input: &str) -> IResult<&str, Instruction, Error<&str>> {
    alt((
        alt((
            map(
                tuple((
                    tag_no_case("TR"),
                    multispace1,
                    transfer_arg,
                    right_arrow_sep,
                    transfer_arg,
                )),
                |(_, _, s, _, t)| Instruction::Transfer(s, t),
            ),
            map(
                tuple((
                    tag_no_case("LDW"),
                    multispace1,
                    load_target,
                    left_arrow_sep,
                    load_word_source,
                )),
                |(_, _, t, _, s)| Instruction::LoadWord(t, s),
            ),
            map(
                tuple((
                    tag_no_case("STW"),
                    multispace1,
                    store_source,
                    right_arrow_sep,
                    store_target,
                )),
                |(_, _, s, _, t)| Instruction::StoreWord(s, t),
            ),
            map(
                tuple((
                    tag_no_case("LDB"),
                    multispace1,
                    load_target,
                    left_arrow_sep,
                    load_byte_source,
                )),
                |(_, _, t, _, s)| Instruction::LoadByte(t, s),
            ),
            map(
                tuple((
                    tag_no_case("STB"),
                    multispace1,
                    store_source,
                    right_arrow_sep,
                    store_target,
                )),
                |(_, _, s, _, t)| Instruction::StoreByte(s, t),
            ),
            map(
                tuple((tag_no_case("PUSH"), multispace1, stack_arg)),
                |(_, _, s)| Instruction::Push(s),
            ),
            map(
                tuple((tag_no_case("POP"), multispace1, stack_arg)),
                |(_, _, t)| Instruction::Pop(t),
            ),
            map(
                tuple((tag_no_case("INC"), multispace1, count_target)),
                |(_, _, t)| Instruction::Increment(t),
            ),
            map(
                tuple((tag_no_case("DEC"), multispace1, count_target)),
                |(_, _, t)| Instruction::Decrement(t),
            ),
            map(
                tuple((
                    tag_no_case("ADD"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::Add(l, r, t),
            ),
            map(
                tuple((
                    tag_no_case("ADDC"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::AddWithCarry(l, r, t),
            ),
            map(
                tuple((
                    tag_no_case("SUB"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::Subtract(l, r, t),
            ),
            map(
                tuple((
                    tag_no_case("SUBB"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::SubtractWithBorrow(l, r, t),
            ),
            map(
                tuple((
                    tag_no_case("AND"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::BitwiseAnd(l, r, t),
            ),
            map(
                tuple((
                    tag_no_case("XOR"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::BitwiseXor(l, r, t),
            ),
            map(
                tuple((
                    tag_no_case("OR"),
                    multispace1,
                    alu_source,
                    comma_sep,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, l, _, r, _, t)| Instruction::BitwiseOr(l, r, t),
            ),
        )),
        alt((
            map(
                tuple((
                    tag_no_case("NEG"),
                    multispace1,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, s, _, t)| Instruction::Negate(s, t),
            ),
            map(
                tuple((
                    tag_no_case("NOT"),
                    multispace1,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, s, _, t)| Instruction::BitwiseNot(s, t),
            ),
            map(
                tuple((
                    tag_no_case("SHL"),
                    multispace1,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, s, _, t)| Instruction::ShiftLeft(s, t),
            ),
            map(
                tuple((
                    tag_no_case("SHR"),
                    multispace1,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, s, _, t)| Instruction::ShiftRight(s, t),
            ),
            map(
                tuple((
                    tag_no_case("ROL"),
                    multispace1,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, s, _, t)| Instruction::RotateLeft(s, t),
            ),
            map(
                tuple((
                    tag_no_case("ROR"),
                    multispace1,
                    alu_source,
                    right_arrow_sep,
                    alu_target,
                )),
                |(_, _, s, _, t)| Instruction::RotateRight(s, t),
            ),
            map(
                tuple((
                    alt((tag_no_case("JPEQ"), tag_no_case("JPZ"), tag_no_case("JPF"))),
                    multispace1,
                    jump_target,
                )),
                |(_, _, t)| Instruction::JumpIfEqual(t),
            ),
            map(
                tuple((
                    alt((
                        tag_no_case("JPNEQ"),
                        tag_no_case("JPNZ"),
                        tag_no_case("JPT"),
                    )),
                    multispace1,
                    jump_target,
                )),
                |(_, _, t)| Instruction::JumpIfNotEqual(t),
            ),
            map(
                tuple((tag_no_case("JPG"), multispace1, jump_target)),
                |(_, _, t)| Instruction::JumpIfGreater(t),
            ),
            map(
                tuple((
                    alt((tag_no_case("JPL"), tag_no_case("JPNEG"))),
                    multispace1,
                    jump_target,
                )),
                |(_, _, t)| Instruction::JumpIfLess(t),
            ),
            map(
                tuple((
                    alt((tag_no_case("JPGEQ"), tag_no_case("JPPOS"))),
                    multispace1,
                    jump_target,
                )),
                |(_, _, t)| Instruction::JumpIfGreaterOrEqual(t),
            ),
            map(
                tuple((tag_no_case("JPLEQ"), multispace1, jump_target)),
                |(_, _, t)| Instruction::JumpIfLessOrEqual(t),
            ),
            map(
                tuple((tag_no_case("JPC"), multispace1, jump_target)),
                |(_, _, t)| Instruction::JumpOnCarry(t),
            ),
            map(
                tuple((tag_no_case("JPCC"), multispace1, jump_target)),
                |(_, _, t)| Instruction::JumpOnCarryClear(t),
            ),
            map(
                tuple((tag_no_case("JPO"), multispace1, jump_target)),
                |(_, _, t)| Instruction::JumpOnOverflow(t),
            ),
            map(
                tuple((tag_no_case("JPNO"), multispace1, jump_target)),
                |(_, _, t)| Instruction::JumpOnNoOverflow(t),
            ),
            map(
                tuple((tag_no_case("JP"), multispace1, jump_target)),
                |(_, _, t)| Instruction::Jump(t),
            ),
        )),
        map(
            tuple((
                tag_no_case("CMP"),
                multispace1,
                compare_source,
                comma_sep,
                compare_with_source,
            )),
            |(_, _, l, _, r)| Instruction::Compare(l, r),
        ),
        map(
            tuple((tag_no_case("SSF"), multispace1, status_flags_source)),
            |(_, _, s)| Instruction::SetStatusFlags(s),
        ),
        map(
            tuple((tag_no_case("RSF"), multispace1, status_flags_source)),
            |(_, _, s)| Instruction::ResetStatusFlags(s),
        ),
        map(
            tuple((
                tag_no_case("BIT"),
                multispace1,
                bit_test_source,
                comma_sep,
                bit_test_with_source,
            )),
            |(_, _, l, _, r)| Instruction::BitTest(l, r),
        ),
        map(tag_no_case("BRK"), |_| Instruction::SoftwareInterrupt),
        map(tag_no_case("WAITI"), |_| Instruction::WaitForInterrupt),
    ))(input)
}

fn parameter(input: &str) -> IResult<&str, Parameter, Error<&str>> {
    alt((
        map(index_register, |r| Parameter::IndexRegister(r)),
        map(general_purpose_register, |r| {
            Parameter::GeneralPurposeRegister(r)
        }),
        map(immediate, |v| Parameter::Immediate(v)),
        map(absolute_indexed, |(a, i)| Parameter::AbsoluteIndexed(a, i)),
        map(absolute, |a| Parameter::Absolute(a)),
        map(register_indirect_indexed, |(r, i)| {
            Parameter::RegisterIndirectIndexed(r, i)
        }),
        map(register_indirect, |r| Parameter::RegisterIndirect(r)),
    ))(input)
}

fn directive(input: &str) -> IResult<&str, Directive, Error<&str>> {
    preceded(
        char('.'),
        alt((
            map(
                tuple((tag_no_case("org"), multispace1, expression)),
                |(_, _, expr)| Directive::Origin(expr),
            ),
            map(
                tuple((
                    tag_no_case("def"),
                    multispace1,
                    identifier,
                    equals_sep,
                    expression,
                )),
                |(_, _, name, _, expr)| Directive::Define(name.to_string(), expr),
            ),
            map(
                tuple((
                    tag_no_case("reg"),
                    multispace1,
                    identifier,
                    equals_sep,
                    general_purpose_register,
                )),
                |(_, _, name, _, reg)| Directive::DefineRegister(name.to_string(), reg),
            ),
            map(
                tuple((
                    tag_no_case("byte"),
                    multispace1,
                    separated_list1(comma_sep, expression),
                )),
                |(_, _, exprs)| Directive::StoreByte(DisplayableVec(exprs)),
            ),
            map(
                tuple((
                    tag_no_case("word"),
                    multispace1,
                    separated_list1(comma_sep, expression),
                )),
                |(_, _, exprs)| Directive::StoreWord(DisplayableVec(exprs)),
            ),
            map(
                tuple((tag_no_case("asciiz"), multispace1, string_literal)),
                |(_, _, s)| Directive::AsciiString(s, true),
            ),
            map(
                tuple((tag_no_case("ascii"), multispace1, string_literal)),
                |(_, _, s)| Directive::AsciiString(s, false),
            ),
            map(
                tuple((tag_no_case("unicodez"), multispace1, string_literal)),
                |(_, _, s)| Directive::UnicodeString(s, true),
            ),
            map(
                tuple((tag_no_case("unicode"), multispace1, string_literal)),
                |(_, _, s)| Directive::UnicodeString(s, false),
            ),
            map(
                tuple((
                    tag_no_case("call"),
                    multispace1,
                    identifier,
                    multispace0,
                    char('('),
                    multispace0,
                    separated_list0(comma_sep, parameter),
                    multispace0,
                    char(')'),
                )),
                |(_, _, name, _, _, _, params, _, _)| {
                    Directive::Call(name.to_string(), DisplayableVec(params))
                },
            ),
            map(tag_no_case("return"), |_| Directive::Return),
        )),
    )(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseResult<T> {
    Success(T),
    Failure(PathBuf, usize, String),
}

fn parse_statement(input_line: &InputLine) -> ParseResult<Statement> {
    let blank_result = tuple((multispace0, opt(comment), eof))(&input_line.line);
    match blank_result {
        Ok(_) => ParseResult::Success(Statement::Empty),
        Err(_) => {
            let stm_result = delimited(
                multispace0,
                alt((
                    map(instruction, |inst| Statement::Instruction(inst)),
                    map(directive, |dir| Statement::Directive(dir)),
                    map(delimited(char('@'), identifier, colon_sep), |name| {
                        Statement::Label(name.to_string())
                    }),
                )),
                tuple((multispace0, opt(comment), eof)),
            )(&input_line.line);

            match stm_result {
                Ok((_, stm)) => ParseResult::Success(stm),
                Err(_) => ParseResult::Failure(
                    input_line.source.clone(),
                    input_line.line_number,
                    "Unknown statement".to_string(),
                ),
            }
        }
    }
}

fn parse_function_open(input_line: &InputLine) -> Option<(String, DisplayableVec<String>)> {
    let result = delimited(
        multispace0,
        tuple((
            tag_no_case(".fn"),
            multispace1,
            identifier,
            multispace0,
            delimited(
                char('('),
                delimited(
                    multispace0,
                    separated_list0(comma_sep, identifier),
                    multispace0,
                ),
                char(')'),
            ),
        )),
        tuple((multispace0, opt(comment), eof)),
    )(&input_line.line);
    match result {
        Ok((_, (_, _, name, _, params))) => Some((
            name.to_string(),
            DisplayableVec(params.into_iter().map(|s| s.to_string()).collect()),
        )),
        Err(_) => None,
    }
}

fn parse_block_directive(input_line: &InputLine, tag: &str) -> bool {
    let result = delimited(
        multispace0,
        tag_no_case(tag),
        tuple((multispace0, opt(comment), eof)),
    )(&input_line.line);
    match result {
        Ok((_, _)) => true,
        Err(_) => false,
    }
}

fn parse_function_close(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".endfn")
}

fn parse_main_entry_point_open(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".main")
}

fn parse_main_entry_point_close(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".endmain")
}

fn parse_irq_entry_point_open(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".irq")
}

fn parse_irq_entry_point_close(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".endirq")
}

fn parse_nmi_entry_point_open(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".nmi")
}

fn parse_nmi_entry_point_close(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".endnmi")
}

fn parse_brk_entry_point_open(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".brk")
}

fn parse_brk_entry_point_close(input_line: &InputLine) -> bool {
    parse_block_directive(input_line, ".endbrk")
}

fn parse_include(input_line: &InputLine) -> Option<PathBuf> {
    let result = delimited(
        multispace0,
        preceded(
            tuple((tag_no_case(".include"), multispace1)),
            string_literal,
        ),
        tuple((multispace0, opt(comment), eof)),
    )(&input_line.line);

    match result {
        Ok((_, path)) => Some(PathBuf::from(path)),
        Err(_) => None,
    }
}

struct InputLine {
    line: String,
    line_number: usize,
    source: PathBuf,
}
impl InputLine {
    fn new(line: String, line_number: usize, source: PathBuf) -> Self {
        Self {
            line,
            line_number,
            source,
        }
    }
}

fn read_lines(file_path: &Path) -> ParseResult<Vec<InputLine>> {
    match File::open(file_path) {
        Ok(mut file) => {
            let mut asm = String::new();
            match file.read_to_string(&mut asm) {
                Ok(_) => {
                    let parent_dir = file_path.parent().unwrap();
                    let mut lines: Vec<InputLine> = asm
                        .lines()
                        .enumerate()
                        .map(|(line_number, line)| {
                            InputLine::new(line.to_string(), line_number + 1, PathBuf::from(file_path))
                        })
                        .collect();

                    let mut i = lines.len();
                    while i > 0 {
                        i -= 1;

                        let line = lines.get(i).unwrap();
                        if let Some(include_path) = parse_include(line) {
                            let mut full_include_path = PathBuf::new();
                            full_include_path.push(parent_dir);
                            full_include_path.push(include_path);

                            match read_lines(&full_include_path) {
                                ParseResult::Success(sub_lines) => {
                                    lines.splice(i..=i, sub_lines);
                                }
                                ParseResult::Failure(source, row, msg) => {
                                    return ParseResult::Failure(source, row, msg)
                                }
                            }
                        }
                    }
                    ParseResult::Success(lines)
                }
                Err(err) => ParseResult::Failure(
                    PathBuf::from(file_path),
                    0,
                    format!("Unable to read file '{}':\n{}", file_path.display(), err),
                ),
            }
        }
        Err(err) => ParseResult::Failure(
            PathBuf::from(file_path),
            0,
            format!("Unable to open file '{}':\n{}", file_path.display(), err),
        ),
    }
}

pub fn parse_file(file_path: &Path) -> ParseResult<Program> {
    match read_lines(file_path) {
        ParseResult::Success(lines) => {
            let mut prog = Program::new();

            let mut i = 0;
            loop {
                macro_rules! parse_block {
                    ($parse_close_directive:expr) => {{
                        let mut block_body: Vec<Statement> = Vec::new();
                        i += 1;

                        loop {
                            if let Some(sub_line) = lines.get(i) {
                                if $parse_close_directive(sub_line) {
                                    i += 1;
                                    break;
                                } else {
                                    let result = parse_statement(sub_line);
                                    match result {
                                        ParseResult::Success(stm) => {
                                            block_body.push(stm);
                                            i += 1;
                                        }
                                        ParseResult::Failure(source, row, msg) => {
                                            return ParseResult::Failure(source, row, msg)
                                        }
                                    }
                                }
                            } else {
                                return ParseResult::Failure(
                                    PathBuf::from(file_path),
                                    i + 1,
                                    "Function was not closed".to_string(),
                                );
                            }
                        }

                        ParseResult::Success(block_body)
                    }};
                }

                if let Some(line) = lines.get(i) {
                    if let Some((name, params)) = parse_function_open(line) {
                        match parse_block!(parse_function_close) {
                            ParseResult::Success(body) => {
                                prog.body.push(TopLevelStatement::Function(
                                    name.to_string(),
                                    params,
                                    body,
                                ));
                            }
                            ParseResult::Failure(source, row, msg) => {
                                return ParseResult::Failure(source, row, msg)
                            }
                        }
                    } else if parse_main_entry_point_open(line) {
                        match parse_block!(parse_main_entry_point_close) {
                            ParseResult::Success(body) => {
                                prog.body.push(TopLevelStatement::MainEntryPoint(body));
                            }
                            ParseResult::Failure(source, row, msg) => {
                                return ParseResult::Failure(source, row, msg)
                            }
                        }
                    } else if parse_irq_entry_point_open(line) {
                        match parse_block!(parse_irq_entry_point_close) {
                            ParseResult::Success(body) => {
                                prog.body.push(TopLevelStatement::IrqEntryPoint(body));
                            }
                            ParseResult::Failure(source, row, msg) => {
                                return ParseResult::Failure(source, row, msg)
                            }
                        }
                    } else if parse_nmi_entry_point_open(line) {
                        match parse_block!(parse_nmi_entry_point_close) {
                            ParseResult::Success(body) => {
                                prog.body.push(TopLevelStatement::NmiEntryPoint(body));
                            }
                            ParseResult::Failure(source, row, msg) => {
                                return ParseResult::Failure(source, row, msg)
                            }
                        }
                    } else if parse_brk_entry_point_open(line) {
                        match parse_block!(parse_brk_entry_point_close) {
                            ParseResult::Success(body) => {
                                prog.body.push(TopLevelStatement::BrkEntryPoint(body));
                            }
                            ParseResult::Failure(source, row, msg) => {
                                return ParseResult::Failure(source, row, msg)
                            }
                        }
                    } else {
                        let result = parse_statement(line);
                        match result {
                            ParseResult::Success(stm) => {
                                prog.body.push(TopLevelStatement::Statement(stm));
                                i += 1;
                            }
                            ParseResult::Failure(source, row, msg) => {
                                return ParseResult::Failure(source, row, msg)
                            }
                        }
                    }
                } else {
                    break;
                }
            }

            ParseResult::Success(prog)
        }
        ParseResult::Failure(source, row, msg) => ParseResult::Failure(source, row, msg),
    }
}
