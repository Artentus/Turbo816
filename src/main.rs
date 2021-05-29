#[allow(non_snake_case)]
mod T816;
#[allow(non_snake_case)]
mod W65C816;

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::{Error, ParseError};
use nom::multi::*;
use nom::sequence::*;
use nom::{IResult, Parser};
use std::fmt::{Binary, Debug, Display, LowerHex, Octal, UpperHex};
use std::fs::File;
use std::io::prelude::*;
use std::num::Wrapping;
use std::path::Path;

pub type HashMap<TKey, TValue> = ahash::AHashMap<TKey, TValue>;
pub type HashSet<T> = ahash::AHashSet<T>;

#[derive(PartialEq, Eq, Clone)]
pub struct DisplayableVec<T>(Vec<T>)
where
    T: Display + Debug;
impl DisplayableVec<Expression> {
    pub fn to_hex(&self, byte_size: usize, add_at_symbol: bool) -> String {
        let mut s = String::new();

        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            let item_str = format!("{}", item.to_hex(byte_size, add_at_symbol));
            s.push_str(&item_str);
        }

        s
    }
}
impl<T> Display for DisplayableVec<T>
where
    T: Display + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            let item_str = format!("{}", item);
            s.push_str(&item_str);
        }

        write!(f, "{}", s)
    }
}
impl<T> Debug for DisplayableVec<T>
where
    T: Display + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
impl<T> LowerHex for DisplayableVec<T>
where
    T: Display + Debug + LowerHex,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            let item_str = format!("0x{:x}", item);
            s.push_str(&item_str);
        }

        write!(f, "{}", s)
    }
}
impl<T> UpperHex for DisplayableVec<T>
where
    T: Display + Debug + UpperHex,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            let item_str = format!("0x{:X}", item);
            s.push_str(&item_str);
        }

        write!(f, "{}", s)
    }
}
impl<T> Octal for DisplayableVec<T>
where
    T: Display + Debug + Octal,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            let item_str = format!("0o{:o}", item);
            s.push_str(&item_str);
        }

        write!(f, "{}", s)
    }
}
impl<T> Binary for DisplayableVec<T>
where
    T: Display + Debug + Binary,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            let item_str = format!("0b{:b}", item);
            s.push_str(&item_str);
        }

        write!(f, "{}", s)
    }
}

pub fn discard<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, (), E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    E: ParseError<I>,
{
    move |i: I| match f.parse(i.clone()) {
        Err(e) => return Err(e),
        Ok((i1, _)) => Ok((i1, ())),
    }
}

pub fn then_return<I, O, E, F, R>(mut f: F, value: R) -> impl FnMut(I) -> IResult<I, R, E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    E: ParseError<I>,
    R: Clone,
{
    move |input: I| {
        let (input, _) = f.parse(input)?;
        Ok((input, value.clone()))
    }
}

fn decimal_digit(input: &str) -> IResult<&str, char, Error<&str>> {
    one_of("0123456789")(input)
}
fn decimal_digit_run(input: &str) -> IResult<&str, String, Error<&str>> {
    map(
        recognize(tuple((decimal_digit, many0(one_of("_0123456789"))))),
        |s| s.replace("_", ""),
    )(input)
}

fn hexadecimal_digit(input: &str) -> IResult<&str, char, Error<&str>> {
    one_of("0123456789ABCDEFabcdef")(input)
}
fn hexadecimal_digit_run(input: &str) -> IResult<&str, String, Error<&str>> {
    map(
        recognize(tuple((
            hexadecimal_digit,
            many0(one_of("_0123456789ABCDEFabcdef")),
        ))),
        |s| s.replace("_", ""),
    )(input)
}

fn octal_digit(input: &str) -> IResult<&str, char, Error<&str>> {
    one_of("01234567")(input)
}
fn octal_digit_run(input: &str) -> IResult<&str, String, Error<&str>> {
    map(
        recognize(tuple((octal_digit, many0(one_of("_01234567"))))),
        |s| s.replace("_", ""),
    )(input)
}

fn binary_digit(input: &str) -> IResult<&str, char, Error<&str>> {
    one_of("01")(input)
}
fn binary_digit_run(input: &str) -> IResult<&str, String, Error<&str>> {
    map(
        recognize(tuple((binary_digit, many0(one_of("_01"))))),
        |s| s.replace("_", ""),
    )(input)
}

fn hexadecimal_prefix(input: &str) -> IResult<&str, &str, Error<&str>> {
    alt((tag("0x"), tag("0X")))(input)
}
fn octal_prefix(input: &str) -> IResult<&str, &str, Error<&str>> {
    alt((tag("0o"), tag("0O")))(input)
}
fn binary_prefix(input: &str) -> IResult<&str, &str, Error<&str>> {
    alt((tag("0b"), tag("0B")))(input)
}

fn decimal_number(input: &str) -> IResult<&str, isize, Error<&str>> {
    map(decimal_digit_run, |digits| {
        isize::from_str_radix(&digits, 10).unwrap()
    })(input)
}
fn hexadecimal_number(input: &str) -> IResult<&str, isize, Error<&str>> {
    map(hexadecimal_digit_run, |digits| {
        isize::from_str_radix(&digits, 16).unwrap()
    })(input)
}
fn octal_number(input: &str) -> IResult<&str, isize, Error<&str>> {
    map(octal_digit_run, |digits| {
        isize::from_str_radix(&digits, 8).unwrap()
    })(input)
}
fn binary_number(input: &str) -> IResult<&str, isize, Error<&str>> {
    map(binary_digit_run, |digits| {
        isize::from_str_radix(&digits, 2).unwrap()
    })(input)
}

fn number(input: &str) -> IResult<&str, isize, Error<&str>> {
    alt((
        preceded(hexadecimal_prefix, hexadecimal_number),
        preceded(octal_prefix, octal_number),
        preceded(binary_prefix, binary_number),
        decimal_number,
    ))(input)
}

fn letter(input: &str) -> IResult<&str, char, Error<&str>> {
    one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")(input)
}
fn identifier(input: &str) -> IResult<&str, &str, Error<&str>> {
    recognize(tuple((
        alt((letter, char('_'))),
        many0(alt((letter, decimal_digit, char('_')))),
    )))(input)
}

fn string_literal(input: &str) -> IResult<&str, &str, Error<&str>> {
    recognize(delimited(char('"'), take_while(|c| c != '"'), char('"')))(input)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOperatorType {
    Positive,
    Negative,
    BitwiseNot,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOperatorType {
    Multiply,
    Divide,
    Remainder,
    Add,
    Subtract,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Literal(Wrapping<isize>),
    Label(String),
    Identifier(String),
    UnaryOperator(UnaryOperatorType, Box<Expression>),
    BinaryOperator(BinaryOperatorType, Box<Expression>, Box<Expression>),
}
impl Expression {
    fn fmt_with_number_format(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        format_number: fn(
            &mut std::fmt::Formatter<'_>,
            value: &Wrapping<isize>,
        ) -> std::fmt::Result,
    ) -> std::fmt::Result {
        match self {
            Expression::Literal(value) => format_number(f, value),
            Expression::Label(name) => {
                write!(f, "@{}", name)
            }
            Expression::Identifier(name) => {
                write!(f, "{}", name)
            }
            Expression::UnaryOperator(op_type, expr) => match op_type {
                UnaryOperatorType::Positive => {
                    write!(f, "+{}", expr)
                }
                UnaryOperatorType::Negative => {
                    write!(f, "-{}", expr)
                }
                UnaryOperatorType::BitwiseNot => {
                    write!(f, "~{}", expr)
                }
            },
            Expression::BinaryOperator(op_type, left, right) => {
                let mut left_str = format!("{}", left);
                let mut right_str = format!("{}", right);
                if left_str.contains(' ') {
                    left_str = format!("({})", left);
                }
                if right_str.contains(' ') {
                    right_str = format!("({})", right);
                }

                match op_type {
                    BinaryOperatorType::Multiply => {
                        write!(f, "{} * {}", left_str, right_str)
                    }
                    BinaryOperatorType::Divide => {
                        write!(f, "{} / {}", left_str, right_str)
                    }
                    BinaryOperatorType::Remainder => {
                        write!(f, "{} % {}", left_str, right_str)
                    }
                    BinaryOperatorType::Add => {
                        write!(f, "{} + {}", left_str, right_str)
                    }
                    BinaryOperatorType::Subtract => {
                        write!(f, "{} - {}", left_str, right_str)
                    }
                    BinaryOperatorType::ShiftLeft => {
                        write!(f, "{} << {}", left_str, right_str)
                    }
                    BinaryOperatorType::ShiftRight => {
                        write!(f, "{} >> {}", left_str, right_str)
                    }
                    BinaryOperatorType::BitwiseAnd => {
                        write!(f, "{} & {}", left_str, right_str)
                    }
                    BinaryOperatorType::BitwiseXor => {
                        write!(f, "{} ^ {}", left_str, right_str)
                    }
                    BinaryOperatorType::BitwiseOr => {
                        write!(f, "{} | {}", left_str, right_str)
                    }
                }
            }
        }
    }

    pub fn to_hex(&self, byte_size: usize, add_at_symbol: bool) -> String {
        match self {
            Expression::Literal(value) => match byte_size {
                1 => format!("0x{0:0>2X}", value.0 & 0x000000FF),
                2 => format!("0x{0:0>4X}", value.0 & 0x0000FFFF),
                3 => format!("0x{0:0>6X}", value.0 & 0x00FFFFFF),
                _ => format!("0x{0:0>1$X}", value.0, byte_size * 2),
            },
            Expression::Label(name) => {
                if add_at_symbol {
                    format!("@{}", name)
                } else {
                    format!("{}", name)
                }
            }
            Expression::Identifier(name) => {
                format!("{}", name)
            }
            Expression::UnaryOperator(op_type, expr) => match op_type {
                UnaryOperatorType::Positive => {
                    format!("+{}", expr.to_hex(byte_size, add_at_symbol))
                }
                UnaryOperatorType::Negative => {
                    format!("-{}", expr.to_hex(byte_size, add_at_symbol))
                }
                UnaryOperatorType::BitwiseNot => {
                    format!("~{}", expr.to_hex(byte_size, add_at_symbol))
                }
            },
            Expression::BinaryOperator(op_type, left, right) => {
                let mut left_str = format!("{}", left.to_hex(byte_size, add_at_symbol));
                let mut right_str = format!("{}", right.to_hex(byte_size, add_at_symbol));
                if left_str.contains(' ') {
                    left_str = format!("({})", left.to_hex(byte_size, add_at_symbol));
                }
                if right_str.contains(' ') {
                    right_str = format!("({})", right.to_hex(byte_size, add_at_symbol));
                }

                match op_type {
                    BinaryOperatorType::Multiply => {
                        format!("{} * {}", left_str, right_str)
                    }
                    BinaryOperatorType::Divide => {
                        format!("{} / {}", left_str, right_str)
                    }
                    BinaryOperatorType::Remainder => {
                        format!("{} % {}", left_str, right_str)
                    }
                    BinaryOperatorType::Add => {
                        format!("{} + {}", left_str, right_str)
                    }
                    BinaryOperatorType::Subtract => {
                        format!("{} - {}", left_str, right_str)
                    }
                    BinaryOperatorType::ShiftLeft => {
                        format!("{} << {}", left_str, right_str)
                    }
                    BinaryOperatorType::ShiftRight => {
                        format!("{} >> {}", left_str, right_str)
                    }
                    BinaryOperatorType::BitwiseAnd => {
                        format!("{} & {}", left_str, right_str)
                    }
                    BinaryOperatorType::BitwiseXor => {
                        format!("{} ^ {}", left_str, right_str)
                    }
                    BinaryOperatorType::BitwiseOr => {
                        format!("{} | {}", left_str, right_str)
                    }
                }
            }
        }
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_number_format(f, |f1, value| write!(f1, "{}", value.0))
    }
}
impl LowerHex for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_number_format(f, |f1, value| write!(f1, "0x{:x}", value.0))
    }
}
impl UpperHex for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_number_format(f, |f1, value| write!(f1, "0x{:X}", value.0))
    }
}
impl Octal for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_number_format(f, |f1, value| write!(f1, "0o{:o}", value.0))
    }
}
impl Binary for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_number_format(f, |f1, value| write!(f1, "0b{:b}", value.0))
    }
}

fn number_literal(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(number, |v| Expression::Literal(Wrapping(v)))(input)
}
fn label(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(preceded(char('@'), identifier), |n| {
        Expression::Label(n.to_string())
    })(input)
}

fn unary_op(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        tuple((
            terminated(alt((char('+'), char('-'), char('~'))), multispace0),
            sub_expression,
        )),
        |(op, expr)| {
            Expression::UnaryOperator(
                if op == '+' {
                    UnaryOperatorType::Positive
                } else if op == '-' {
                    UnaryOperatorType::Negative
                } else {
                    UnaryOperatorType::BitwiseNot
                },
                Box::new(expr),
            )
        },
    )(input)
}

fn product(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        tuple((
            alt((unary_op, sub_expression)),
            many0(tuple((
                delimited(
                    multispace0,
                    alt((char('*'), char('/'), char('%'))),
                    multispace0,
                ),
                alt((unary_op, sub_expression)),
            ))),
        )),
        |(first_term, terms)| {
            terms.into_iter().fold(first_term, |a, b| {
                Expression::BinaryOperator(
                    if b.0 == '*' {
                        BinaryOperatorType::Multiply
                    } else if b.0 == '/' {
                        BinaryOperatorType::Divide
                    } else {
                        BinaryOperatorType::Remainder
                    },
                    Box::new(a),
                    Box::new(b.1),
                )
            })
        },
    )(input)
}

fn term(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        tuple((
            product,
            many0(tuple((
                delimited(multispace0, alt((char('+'), char('-'))), multispace0),
                product,
            ))),
        )),
        |(first_term, terms)| {
            terms.into_iter().fold(first_term, |a, b| {
                Expression::BinaryOperator(
                    if b.0 == '+' {
                        BinaryOperatorType::Add
                    } else {
                        BinaryOperatorType::Subtract
                    },
                    Box::new(a),
                    Box::new(b.1),
                )
            })
        },
    )(input)
}

fn shift_term(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        tuple((
            term,
            many0(tuple((
                delimited(multispace0, alt((tag("<<"), tag(">>"))), multispace0),
                term,
            ))),
        )),
        |(first_term, terms)| {
            terms.into_iter().fold(first_term, |a, b| {
                Expression::BinaryOperator(
                    if b.0 == "<<" {
                        BinaryOperatorType::ShiftLeft
                    } else {
                        BinaryOperatorType::ShiftRight
                    },
                    Box::new(a),
                    Box::new(b.1),
                )
            })
        },
    )(input)
}

fn bitwise_product(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        separated_list1(delimited(multispace0, char('&'), multispace0), shift_term),
        |terms| {
            terms
                .into_iter()
                .reduce(|a, b| {
                    Expression::BinaryOperator(
                        BinaryOperatorType::BitwiseAnd,
                        Box::new(a),
                        Box::new(b),
                    )
                })
                .unwrap()
        },
    )(input)
}

fn bitwise_xor_term(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        separated_list1(
            delimited(multispace0, char('^'), multispace0),
            bitwise_product,
        ),
        |terms| {
            terms
                .into_iter()
                .reduce(|a, b| {
                    Expression::BinaryOperator(
                        BinaryOperatorType::BitwiseXor,
                        Box::new(a),
                        Box::new(b),
                    )
                })
                .unwrap()
        },
    )(input)
}

fn bitwise_or_term(input: &str) -> IResult<&str, Expression, Error<&str>> {
    map(
        separated_list1(
            delimited(multispace0, char('|'), multispace0),
            bitwise_xor_term,
        ),
        |terms| {
            terms
                .into_iter()
                .reduce(|a, b| {
                    Expression::BinaryOperator(
                        BinaryOperatorType::BitwiseOr,
                        Box::new(a),
                        Box::new(b),
                    )
                })
                .unwrap()
        },
    )(input)
}

fn sub_expression(input: &str) -> IResult<&str, Expression, Error<&str>> {
    alt((
        delimited(
            char('('),
            delimited(multispace0, expression, multispace0),
            char(')'),
        ),
        map(identifier, |name| Expression::Identifier(name.to_string())),
        label,
        number_literal,
    ))(input)
}

fn expression(input: &str) -> IResult<&str, Expression, Error<&str>> {
    bitwise_or_term(input)
}

fn compile_and_assemble(input_path: &Path, output_path: &Path) {
    match File::open(input_path) {
        Ok(mut input_file) => {
            let mut asm = String::new();
            match input_file.read_to_string(&mut asm) {
                Ok(_) => {
                    let result = T816::grammar::parse_program(&asm);
                    match result {
                        T816::grammar::ParseResult::Success(prog) => {
                            println!("{}", prog);

                            let comp_result = T816::compile::compile_program(&prog);
                            match comp_result {
                                T816::compile::CompileResult::Success(comp_prog) => {
                                    println!("{}", comp_prog);

                                    let assembly = W65C816::assemble_program(&comp_prog);
                                    match File::create(output_path) {
                                        Ok(mut output_file) => {
                                            match output_file.write_all(&assembly.0) {
                                                Ok(_) => {
                                                    println!("Program successfully compiled and assembled to '{}'", output_path.display());
                                                }
                                                Err(msg) => {
                                                    println!(
                                                        "Unable to write to file '{}':\n{}",
                                                        output_path.display(),
                                                        msg
                                                    );
                                                }
                                            }
                                        }
                                        Err(msg) => {
                                            println!(
                                                "Unable to create file '{}':\n{}",
                                                output_path.display(),
                                                msg
                                            );
                                        }
                                    }
                                }
                                T816::compile::CompileResult::Failure(msg) => {
                                    println!("Compile error:\n{}", msg)
                                }
                            }
                        }
                        T816::grammar::ParseResult::Failure(line, msg) => {
                            println!("Parse error in line {}:\n{}", line, msg)
                        }
                    }
                }
                Err(msg) => {
                    println!("Unable to read file '{}':\n{}", input_path.display(), msg);
                }
            }
        }
        Err(msg) => {
            println!("Unable to open file '{}':\n{}", input_path.display(), msg);
        }
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    match args.get(1) {
        Some(input_arg) => match args.get(2) {
            Some(output_arg) => {
                let input_path = Path::new(input_arg);
                let output_path = Path::new(output_arg);

                compile_and_assemble(input_path, output_path);
            }
            None => {
                println!("Please specify output file path");
            }
        },
        None => {
            println!("Please specify input file path");
        }
    }
}
