use super::*;
use crate::*;

// '%' is not a valid char for the programmer to put into
// a label name so these are guaranteed to be unique
const FUNCTION_PREFIX: &str = "%_FUNCTION_";
const MAIN_FUNCTION_LABEL: &str = "%_MAIN";
const MAIN_LOOP_LABEL: &str = "%_MAIN_LOOP";
const SCOPE_LABEL_PREFIX: &str = "%_SCOPE_";
const IRQ_FUNCTION_LABEL: &str = "%_IRQ";
const NMI_FUNCTION_LABEL: &str = "%_NMI";
const BRK_FUNCTION_LABEL: &str = "%_BRK";
const JUMP_LABEL_PREFIX: &str = "%_JUMP_";
const EMPTY_INTERRUPT_LABEL: &str = "%_EMPTY_INTERRUPT";

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompileResult<T> {
    Success(T),
    Failure(String),
}

struct BlockIdentifierCollection {
    defines: HashSet<String>,
    labels: HashSet<String>,
}
impl BlockIdentifierCollection {
    fn new() -> Self {
        Self {
            defines: HashSet::new(),
            labels: HashSet::new(),
        }
    }

    fn contains(&self, ident: &str) -> bool {
        self.defines.contains(ident) || self.labels.contains(ident)
    }
}

struct IdentifierCollection {
    defines: HashSet<String>,
    labels: HashSet<String>,
    functions: HashMap<String, (usize, BlockIdentifierCollection)>,
    main_entry_point: Option<BlockIdentifierCollection>,
    irq_entry_point: Option<BlockIdentifierCollection>,
    nmi_entry_point: Option<BlockIdentifierCollection>,
    brk_entry_point: Option<BlockIdentifierCollection>,
}
impl IdentifierCollection {
    fn new() -> Self {
        Self {
            defines: HashSet::new(),
            labels: HashSet::new(),
            functions: HashMap::new(),
            main_entry_point: None,
            irq_entry_point: None,
            nmi_entry_point: None,
            brk_entry_point: None,
        }
    }

    fn contains(&self, ident: &str) -> bool {
        self.defines.contains(ident)
            || self.labels.contains(ident)
            || self.functions.contains_key(ident)
    }
}

fn preprocess_block_statement_identifier(
    stm: &Statement,
    result: &IdentifierCollection,
    block_result: &mut BlockIdentifierCollection,
) -> CompileResult<()> {
    match stm {
        Statement::Directive(dir) => match dir {
            Directive::Define(name, _) => {
                if result.contains(name) || block_result.contains(name) {
                    return CompileResult::Failure(format!("'{}' is defined twice", name));
                } else {
                    block_result.defines.insert(name.clone());
                }
            }
            Directive::DefineRegister(name, _) => {
                if result.contains(name) || block_result.contains(name) {
                    return CompileResult::Failure(format!("'{}' is defined twice", name));
                } else {
                    block_result.defines.insert(name.clone());
                }
            }
            _ => {}
        },
        Statement::Label(name) => {
            if result.contains(name) || block_result.contains(name) {
                return CompileResult::Failure(format!("'{}' is defined twice", name));
            } else {
                block_result.labels.insert(name.clone());
            }
        }
        _ => {}
    }

    CompileResult::Success(())
}

fn preprocess_identifiers(prog: &T816::Program) -> CompileResult<IdentifierCollection> {
    let mut result = IdentifierCollection::new();

    for tlstm in prog.body.iter() {
        match tlstm {
            TopLevelStatement::Statement(stm) => match stm {
                Statement::Directive(dir) => match dir {
                    Directive::Define(name, _) => {
                        if result.contains(name) {
                            return CompileResult::Failure(format!("'{}' is defined twice", name));
                        } else {
                            result.defines.insert(name.clone());
                        }
                    }
                    Directive::DefineRegister(name, _) => {
                        if result.contains(name) {
                            return CompileResult::Failure(format!("'{}' is defined twice", name));
                        } else {
                            result.defines.insert(name.clone());
                        }
                    }
                    _ => {}
                },
                Statement::Label(name) => {
                    if result.contains(name) {
                        return CompileResult::Failure(format!("'{}' is defined twice", name));
                    } else {
                        result.labels.insert(name.clone());
                    }
                }
                _ => {}
            },
            TopLevelStatement::Function(name, params, body) => {
                if result.contains(name) {
                    return CompileResult::Failure(format!("'{}' is defined twice", name));
                } else {
                    let param_count = params.0.len();
                    if param_count > ARGUMENT_REGISTER_COUNT {
                        return CompileResult::Failure(format!(
                            "Function '{}' has too many arguments",
                            name
                        ));
                    }

                    let mut block_result = BlockIdentifierCollection::new();

                    for p in params.0.iter() {
                        if result.contains(p) {
                            return CompileResult::Failure(format!("'{}' is defined twice", name));
                        } else {
                            block_result.defines.insert(p.clone());
                        }
                    }

                    for stm in body.iter() {
                        if let CompileResult::Failure(msg) =
                            preprocess_block_statement_identifier(stm, &result, &mut block_result)
                        {
                            return CompileResult::Failure(msg);
                        }
                    }

                    result
                        .functions
                        .insert(name.clone(), (param_count, block_result));
                }
            }
            TopLevelStatement::MainEntryPoint(body) => match result.main_entry_point {
                Some(_) => {
                    return CompileResult::Failure("Main function is defined twice".to_string())
                }
                None => {
                    let mut block_result = BlockIdentifierCollection::new();

                    for stm in body.iter() {
                        if let CompileResult::Failure(msg) =
                            preprocess_block_statement_identifier(stm, &result, &mut block_result)
                        {
                            return CompileResult::Failure(msg);
                        }
                    }

                    result.main_entry_point = Some(block_result);
                }
            },
            TopLevelStatement::IrqEntryPoint(body) => match result.irq_entry_point {
                Some(_) => {
                    return CompileResult::Failure("IRQ function is defined twice".to_string())
                }
                None => {
                    let mut block_result = BlockIdentifierCollection::new();

                    for stm in body.iter() {
                        if let CompileResult::Failure(msg) =
                            preprocess_block_statement_identifier(stm, &result, &mut block_result)
                        {
                            return CompileResult::Failure(msg);
                        }
                    }

                    result.irq_entry_point = Some(block_result);
                }
            },
            TopLevelStatement::NmiEntryPoint(body) => match result.nmi_entry_point {
                Some(_) => {
                    return CompileResult::Failure("NMI function is defined twice".to_string())
                }
                None => {
                    let mut block_result = BlockIdentifierCollection::new();

                    for stm in body.iter() {
                        if let CompileResult::Failure(msg) =
                            preprocess_block_statement_identifier(stm, &result, &mut block_result)
                        {
                            return CompileResult::Failure(msg);
                        }
                    }

                    result.nmi_entry_point = Some(block_result);
                }
            },
            TopLevelStatement::BrkEntryPoint(body) => match result.brk_entry_point {
                Some(_) => {
                    return CompileResult::Failure("BRK function is defined twice".to_string())
                }
                None => {
                    let mut block_result = BlockIdentifierCollection::new();

                    for stm in body.iter() {
                        if let CompileResult::Failure(msg) =
                            preprocess_block_statement_identifier(stm, &result, &mut block_result)
                        {
                            return CompileResult::Failure(msg);
                        }
                    }

                    result.brk_entry_point = Some(block_result);
                }
            },
        }
    }

    CompileResult::Success(result)
}

macro_rules! set_m_short {
    () => {
        W65C816::Instruction::SepImmediate(M_FLAG.clone())
    };
}
macro_rules! set_m_long {
    () => {
        W65C816::Instruction::RepImmediate(M_FLAG.clone())
    };
}

#[derive(Debug, Clone)]
pub struct CompileState {
    defines: HashMap<String, Expression>,
    reg_defines: HashMap<String, GeneralPurposeRegister>,
}
impl CompileState {
    fn new() -> Self {
        Self {
            defines: HashMap::new(),
            reg_defines: HashMap::new(),
        }
    }
}

fn get_register_address(
    reg: &GeneralPurposeRegister,
    reg_defines: &HashMap<String, GeneralPurposeRegister>,
) -> CompileResult<Expression> {
    match reg {
        GeneralPurposeRegister::Argument(i) => {
            CompileResult::Success(Expression::Literal(Wrapping((i * 2) as isize)))
        }
        GeneralPurposeRegister::Result(i) => CompileResult::Success(Expression::Literal(Wrapping(
            (i * 2 + ARGUMENT_REGISTER_COUNT) as isize,
        ))),
        GeneralPurposeRegister::Error(i) => CompileResult::Success(Expression::Literal(Wrapping(
            (i * 2 + ARGUMENT_REGISTER_COUNT + RESULT_REGISTER_COUNT) as isize,
        ))),
        GeneralPurposeRegister::Global(i) => CompileResult::Success(Expression::Literal(Wrapping(
            (i * 2 + ARGUMENT_REGISTER_COUNT + RESULT_REGISTER_COUNT + ERROR_REGISTER_COUNT)
                as isize,
        ))),
        GeneralPurposeRegister::Identifier(name) => match reg_defines.get(name) {
            Some(r) => get_register_address(r, reg_defines),
            None => CompileResult::Failure(format!("'{}' is not defined", name)),
        },
    }
}

fn compile_expression(
    expr: &Expression,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Expression> {
    match expr {
        Expression::Literal(value) => CompileResult::Success(Expression::Literal(*value)),
        Expression::Label(name) => {
            if identifiers.contains(name) {
                if identifiers.labels.contains(name) {
                    CompileResult::Success(Expression::Label(name.clone()))
                } else {
                    CompileResult::Failure(format!("'{}' is not a label", name))
                }
            } else {
                match scope_info {
                    Some((scope_name, block_identifiers)) => {
                        if block_identifiers.contains(name) {
                            if block_identifiers.labels.contains(name) {
                                CompileResult::Success(Expression::Label(format!(
                                    "{}{}_{}",
                                    SCOPE_LABEL_PREFIX, scope_name, name
                                )))
                            } else {
                                CompileResult::Failure(format!("'{}' is not a label", name))
                            }
                        } else {
                            CompileResult::Failure(format!("'{}' is not defined", name))
                        }
                    }
                    None => CompileResult::Failure(format!("'{}' is not defined", name)),
                }
            }
        }
        Expression::Identifier(name) => {
            match state.defines.get(name) {
                Some(expr) => compile_expression(expr, identifiers, scope_info, state),
                None => CompileResult::Failure(format!("'{}' is not a defined value", name)),
            }
        }
        Expression::UnaryOperator(op_type, base_expr) => {
            match compile_expression(base_expr, identifiers, scope_info, state) {
                CompileResult::Success(comp_expr) => {
                    CompileResult::Success(Expression::UnaryOperator(*op_type, Box::new(comp_expr)))
                }
                CompileResult::Failure(msg) => CompileResult::Failure(msg),
            }
        }
        Expression::BinaryOperator(op_type, left, right) => {
            match compile_expression(left, identifiers, scope_info, state) {
                CompileResult::Success(comp_left) => {
                    match compile_expression(right, identifiers, scope_info, state) {
                        CompileResult::Success(comp_right) => {
                            CompileResult::Success(Expression::BinaryOperator(
                                *op_type,
                                Box::new(comp_left),
                                Box::new(comp_right),
                            ))
                        }
                        CompileResult::Failure(msg) => CompileResult::Failure(msg),
                    }
                }
                CompileResult::Failure(msg) => CompileResult::Failure(msg),
            }
        }
    }
}

fn compile_expression_list(
    exprs: &[Expression],
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<Expression>> {
    let mut result: Vec<Expression> = Vec::new();
    for expr in exprs.iter() {
        match compile_expression(expr, identifiers, scope_info, state) {
            CompileResult::Success(comp_expr) => {
                result.push(comp_expr);
            }
            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
        }
    }
    CompileResult::Success(result)
}

fn compile_transfer_instruction(
    source: &TransferArg,
    target: &TransferArg,
    _identifiers: &IdentifierCollection,
    _scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match source {
        TransferArg::IndexRegister(si) => match target {
            TransferArg::IndexRegister(ti) => match si {
                IndexRegister::X => match ti {
                    IndexRegister::X => {}
                    IndexRegister::Y => add_instruction!(W65C816::Instruction::TxyImplied),
                },
                IndexRegister::Y => match ti {
                    IndexRegister::X => add_instruction!(W65C816::Instruction::TyxImplied),
                    IndexRegister::Y => {}
                },
            },
            TransferArg::GeneralPurposeRegister(tgp) => {
                let taddr_opt = get_register_address(tgp, &state.reg_defines);
                match taddr_opt {
                    CompileResult::Success(taddr) => match si {
                        IndexRegister::X => {
                            add_instruction!(W65C816::Instruction::StxDirectPage(taddr))
                        }
                        IndexRegister::Y => {
                            add_instruction!(W65C816::Instruction::StyDirectPage(taddr))
                        }
                    },
                    CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                }
            }
            TransferArg::SpecialPurposeRegister(tsp) => match tsp {
                SpecialPurposeRegister::StackPointer => match si {
                    IndexRegister::X => add_instruction!(W65C816::Instruction::TxsImplied),
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TyaImplied);
                        add_instruction!(W65C816::Instruction::TcsImplied);
                    }
                },
                SpecialPurposeRegister::StatusFlags => {
                    match si {
                        IndexRegister::X => add_instruction!(W65C816::Instruction::TxaImplied),
                        IndexRegister::Y => add_instruction!(W65C816::Instruction::TyaImplied),
                    }
                    add_instruction!(set_m_short!());
                    add_instruction!(W65C816::Instruction::PhaImplied);
                    add_instruction!(W65C816::Instruction::PlpImplied);
                    add_instruction!(set_m_long!());
                }
                SpecialPurposeRegister::DataBank => {
                    match si {
                        IndexRegister::X => add_instruction!(W65C816::Instruction::TxaImplied),
                        IndexRegister::Y => add_instruction!(W65C816::Instruction::TyaImplied),
                    }
                    add_instruction!(set_m_short!());
                    add_instruction!(W65C816::Instruction::PhaImplied);
                    add_instruction!(W65C816::Instruction::PlbImplied);
                    add_instruction!(set_m_long!());
                }
            },
        },
        TransferArg::GeneralPurposeRegister(sgp) => {
            let saddr_opt = get_register_address(sgp, &state.reg_defines);
            match saddr_opt {
                CompileResult::Success(saddr) => match target {
                    TransferArg::IndexRegister(ti) => match ti {
                        IndexRegister::X => {
                            add_instruction!(W65C816::Instruction::LdxDirectPage(saddr))
                        }
                        IndexRegister::Y => {
                            add_instruction!(W65C816::Instruction::LdyDirectPage(saddr))
                        }
                    },
                    TransferArg::GeneralPurposeRegister(tgp) => {
                        let taddr_opt = get_register_address(tgp, &state.reg_defines);
                        match taddr_opt {
                            CompileResult::Success(taddr) => {
                                add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                                add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                            }
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    TransferArg::SpecialPurposeRegister(tsp) => match tsp {
                        SpecialPurposeRegister::StackPointer => {
                            add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                            add_instruction!(W65C816::Instruction::TcsImplied);
                        }
                        SpecialPurposeRegister::StatusFlags => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                            add_instruction!(W65C816::Instruction::PhaImplied);
                            add_instruction!(W65C816::Instruction::PlpImplied);
                            add_instruction!(set_m_long!());
                        }
                        SpecialPurposeRegister::DataBank => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                            add_instruction!(W65C816::Instruction::PhaImplied);
                            add_instruction!(W65C816::Instruction::PlbImplied);
                            add_instruction!(set_m_long!());
                        }
                    },
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        TransferArg::SpecialPurposeRegister(ssp) => match ssp {
            SpecialPurposeRegister::StackPointer => match target {
                TransferArg::IndexRegister(ti) => match ti {
                    IndexRegister::X => add_instruction!(W65C816::Instruction::TsxImplied),
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TscImplied);
                        add_instruction!(W65C816::Instruction::TayImplied);
                    }
                },
                TransferArg::GeneralPurposeRegister(tgp) => {
                    let taddr_opt = get_register_address(tgp, &state.reg_defines);
                    match taddr_opt {
                        CompileResult::Success(taddr) => {
                            add_instruction!(W65C816::Instruction::TscImplied);
                            add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                TransferArg::SpecialPurposeRegister(tsp) => match tsp {
                    SpecialPurposeRegister::StackPointer => {}
                    SpecialPurposeRegister::StatusFlags => {
                        add_instruction!(W65C816::Instruction::TscImplied);
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhaImplied);
                        add_instruction!(W65C816::Instruction::PlpImplied);
                        add_instruction!(set_m_long!());
                    }
                    SpecialPurposeRegister::DataBank => {
                        add_instruction!(W65C816::Instruction::TscImplied);
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhaImplied);
                        add_instruction!(W65C816::Instruction::PlbImplied);
                        add_instruction!(set_m_long!());
                    }
                },
            },
            SpecialPurposeRegister::StatusFlags => match target {
                TransferArg::IndexRegister(ti) => match ti {
                    IndexRegister::X => {
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhpImplied);
                        add_instruction!(W65C816::Instruction::PlaImplied);
                        add_instruction!(set_m_long!());
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhpImplied);
                        add_instruction!(W65C816::Instruction::PlaImplied);
                        add_instruction!(set_m_long!());
                        add_instruction!(W65C816::Instruction::TayImplied);
                    }
                },
                TransferArg::GeneralPurposeRegister(tgp) => {
                    let taddr_opt = get_register_address(tgp, &state.reg_defines);
                    match taddr_opt {
                        CompileResult::Success(taddr) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::PhpImplied);
                            add_instruction!(W65C816::Instruction::PlaImplied);
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                TransferArg::SpecialPurposeRegister(tsp) => match tsp {
                    SpecialPurposeRegister::StackPointer => {
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhpImplied);
                        add_instruction!(W65C816::Instruction::PlaImplied);
                        add_instruction!(set_m_long!());
                        add_instruction!(W65C816::Instruction::TcsImplied);
                    }
                    SpecialPurposeRegister::StatusFlags => {}
                    SpecialPurposeRegister::DataBank => {
                        add_instruction!(W65C816::Instruction::PhpImplied);
                        add_instruction!(W65C816::Instruction::PlbImplied);
                    }
                },
            },
            SpecialPurposeRegister::DataBank => match target {
                TransferArg::IndexRegister(ti) => match ti {
                    IndexRegister::X => {
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhbImplied);
                        add_instruction!(W65C816::Instruction::PlaImplied);
                        add_instruction!(set_m_long!());
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhbImplied);
                        add_instruction!(W65C816::Instruction::PlaImplied);
                        add_instruction!(set_m_long!());
                        add_instruction!(W65C816::Instruction::TayImplied);
                    }
                },
                TransferArg::GeneralPurposeRegister(tgp) => {
                    let taddr_opt = get_register_address(tgp, &state.reg_defines);
                    match taddr_opt {
                        CompileResult::Success(taddr) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::PhbImplied);
                            add_instruction!(W65C816::Instruction::PlaImplied);
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                TransferArg::SpecialPurposeRegister(tsp) => match tsp {
                    SpecialPurposeRegister::StackPointer => {
                        add_instruction!(set_m_short!());
                        add_instruction!(W65C816::Instruction::PhbImplied);
                        add_instruction!(W65C816::Instruction::PlaImplied);
                        add_instruction!(set_m_long!());
                        add_instruction!(W65C816::Instruction::TcsImplied);
                    }
                    SpecialPurposeRegister::StatusFlags => {
                        add_instruction!(W65C816::Instruction::PhbImplied);
                        add_instruction!(W65C816::Instruction::PlpImplied);
                    }
                    SpecialPurposeRegister::DataBank => {}
                },
            },
        },
    }

    CompileResult::Success(result)
}

fn compile_load_word_instruction(
    target: &LoadTarget,
    source: &LoadWordSource,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        LoadTarget::IndexRegister(ti) => match ti {
            IndexRegister::X => match source {
                LoadWordSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(W65C816::Instruction::LdxImmediate(comp_value));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::LdxAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(
                                    comp_addr
                                ));
                                add_instruction!(W65C816::Instruction::TaxImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::LdxAbsoluteIndexedY(
                                    comp_addr
                                ));
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                            add_instruction!(W65C816::Instruction::TaxImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::PhyImplied);
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::PlyImplied);
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                            add_instruction!(W65C816::Instruction::TaxImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
            IndexRegister::Y => match source {
                LoadWordSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(W65C816::Instruction::LdyImmediate(comp_value));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::LdyAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::LdyAbsoluteIndexedX(
                                    comp_addr
                                ));
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(
                                    comp_addr
                                ));
                                add_instruction!(W65C816::Instruction::TayImplied);
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                            add_instruction!(W65C816::Instruction::TayImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadWordSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                            add_instruction!(W65C816::Instruction::TayImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
        },
        LoadTarget::GeneralPurposeRegister(tgp) => {
            let taddr_opt = get_register_address(tgp, &state.reg_defines);
            match taddr_opt {
                CompileResult::Success(taddr) => match source {
                    LoadWordSource::Immediate(value) => {
                        if *value == ZERO {
                            add_instruction!(W65C816::Instruction::StzDirectPage(taddr));
                        } else {
                            match compile_expression(value, identifiers, scope_info, state) {
                                CompileResult::Success(comp_value) => {
                                    add_instruction!(W65C816::Instruction::LdaImmediate(
                                        comp_value, false
                                    ));
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                    }
                    LoadWordSource::Absolute(addr) => {
                        match compile_expression(addr, identifiers, scope_info, state) {
                            CompileResult::Success(comp_addr) => {
                                add_instruction!(W65C816::Instruction::LdaAbsolute(comp_addr));
                                add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                            }
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    LoadWordSource::AbsoluteIndexed(addr, index) => {
                        match compile_expression(addr, identifiers, scope_info, state) {
                            CompileResult::Success(comp_addr) => match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(
                                        comp_addr
                                    ));
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(
                                        comp_addr
                                    ));
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                            },
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    LoadWordSource::RegisterIndirect(reg) => {
                        let addr_opt = get_register_address(reg, &state.reg_defines);
                        match addr_opt {
                            CompileResult::Success(addr) => {
                                add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                                add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                            }
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    LoadWordSource::RegisterIndirectIndexed(reg, index) => {
                        let addr_opt = get_register_address(reg, &state.reg_defines);
                        match addr_opt {
                            CompileResult::Success(addr) => match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::PhyImplied);
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::PlyImplied);
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                            },
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_store_word_instruction(
    source: &StoreSource,
    target: &StoreTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match source {
        StoreSource::IndexRegister(si) => match si {
            IndexRegister::X => match target {
                StoreTarget::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::StxAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(
                                        comp_addr
                                    ));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(
                                        comp_addr
                                    ));
                                }
                            }
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::PhyImplied);
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::PlyImplied);
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
            IndexRegister::Y => match target {
                StoreTarget::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::StyAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(
                                        comp_addr
                                    ));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(
                                        comp_addr
                                    ));
                                }
                            }
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::TayImplied);
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
        },
        StoreSource::GeneralPurposeRegister(sgp) => {
            let saddr_opt = get_register_address(sgp, &state.reg_defines);
            match saddr_opt {
                CompileResult::Success(saddr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                    match target {
                        StoreTarget::Absolute(addr) => {
                            match compile_expression(addr, identifiers, scope_info, state) {
                                CompileResult::Success(comp_addr) => {
                                    add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                                }
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                        StoreTarget::AbsoluteIndexed(addr, index) => {
                            match compile_expression(addr, identifiers, scope_info, state) {
                                CompileResult::Success(comp_addr) => match index {
                                    IndexRegister::X => {
                                        add_instruction!(
                                            W65C816::Instruction::StaAbsoluteIndexedX(comp_addr)
                                        );
                                    }
                                    IndexRegister::Y => {
                                        add_instruction!(
                                            W65C816::Instruction::StaAbsoluteIndexedY(comp_addr)
                                        );
                                    }
                                },
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                        StoreTarget::RegisterIndirect(reg) => {
                            let addr_opt = get_register_address(reg, &state.reg_defines);
                            match addr_opt {
                                CompileResult::Success(addr) => {
                                    add_instruction!(W65C816::Instruction::StaDirectPageIndirect(
                                        addr
                                    ));
                                }
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                        StoreTarget::RegisterIndirectIndexed(reg, index) => {
                            let addr_opt = get_register_address(reg, &state.reg_defines);
                            match addr_opt {
                                CompileResult::Success(addr) => match index {
                                    IndexRegister::X => {
                                        add_instruction!(W65C816::Instruction::PhyImplied);
                                        add_instruction!(W65C816::Instruction::TxyImplied);
                                        add_instruction!(
                                            W65C816::Instruction::StaDirectPageIndirectIndexedY(
                                                addr
                                            )
                                        );
                                        add_instruction!(W65C816::Instruction::PlyImplied);
                                    }
                                    IndexRegister::Y => {
                                        add_instruction!(
                                            W65C816::Instruction::StaDirectPageIndirectIndexedY(
                                                addr
                                            )
                                        );
                                    }
                                },
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                    }
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        StoreSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::LdaImmediate(comp_value, false));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }

            match target {
                StoreTarget::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(
                                    comp_addr
                                ));
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(
                                    comp_addr
                                ));
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::PhyImplied);
                                add_instruction!(W65C816::Instruction::TxyImplied);
                                add_instruction!(
                                    W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                );
                                add_instruction!(W65C816::Instruction::PlyImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(
                                    W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                );
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_load_byte_instruction(
    target: &LoadTarget,
    source: &LoadByteSource,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        LoadTarget::IndexRegister(ti) => match ti {
            IndexRegister::X => match source {
                LoadByteSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaImmediate(comp_value, true));
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TaxImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaAbsolute(comp_addr));
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TaxImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(set_m_short!());
                                add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(
                                    comp_addr
                                ));
                                add_instruction!(set_m_long!());
                                add_instruction!(W65C816::Instruction::TaxImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(set_m_short!());
                                add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(
                                    comp_addr
                                ));
                                add_instruction!(set_m_long!());
                                add_instruction!(W65C816::Instruction::TaxImplied);
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TaxImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(set_m_short!());
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::PhyImplied);
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::PlyImplied);
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TaxImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
            IndexRegister::Y => match source {
                LoadByteSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaImmediate(comp_value, true));
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TayImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaAbsolute(comp_addr));
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TayImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(set_m_short!());
                                add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(
                                    comp_addr
                                ));
                                add_instruction!(set_m_long!());
                                add_instruction!(W65C816::Instruction::TayImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(set_m_short!());
                                add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(
                                    comp_addr
                                ));
                                add_instruction!(set_m_long!());
                                add_instruction!(W65C816::Instruction::TayImplied);
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TayImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                LoadByteSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(set_m_short!());
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                            add_instruction!(set_m_long!());
                            add_instruction!(W65C816::Instruction::TayImplied);
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
        },
        LoadTarget::GeneralPurposeRegister(tgp) => {
            let taddr_opt = get_register_address(tgp, &state.reg_defines);
            match taddr_opt {
                CompileResult::Success(taddr) => match source {
                    LoadByteSource::Immediate(value) => {
                        if *value == ZERO {
                            add_instruction!(W65C816::Instruction::StzDirectPage(taddr));
                        } else {
                            match compile_expression(value, identifiers, scope_info, state) {
                                CompileResult::Success(comp_value) => {
                                    add_instruction!(set_m_short!());
                                    add_instruction!(W65C816::Instruction::LdaImmediate(
                                        comp_value, true
                                    ));
                                    add_instruction!(set_m_long!());
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                    }
                    LoadByteSource::Absolute(addr) => {
                        match compile_expression(addr, identifiers, scope_info, state) {
                            CompileResult::Success(comp_addr) => {
                                add_instruction!(set_m_short!());
                                add_instruction!(W65C816::Instruction::LdaAbsolute(comp_addr));
                                add_instruction!(set_m_long!());
                                add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                            }
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    LoadByteSource::AbsoluteIndexed(addr, index) => {
                        match compile_expression(addr, identifiers, scope_info, state) {
                            CompileResult::Success(comp_addr) => match index {
                                IndexRegister::X => {
                                    add_instruction!(set_m_short!());
                                    add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(
                                        comp_addr
                                    ));
                                    add_instruction!(set_m_long!());
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(set_m_short!());
                                    add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(
                                        comp_addr
                                    ));
                                    add_instruction!(set_m_long!());
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                            },
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    LoadByteSource::RegisterIndirect(reg) => {
                        let addr_opt = get_register_address(reg, &state.reg_defines);
                        match addr_opt {
                            CompileResult::Success(addr) => {
                                add_instruction!(set_m_short!());
                                add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                                add_instruction!(set_m_long!());
                                add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                            }
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                    LoadByteSource::RegisterIndirectIndexed(reg, index) => {
                        let addr_opt = get_register_address(reg, &state.reg_defines);
                        match addr_opt {
                            CompileResult::Success(addr) => match index {
                                IndexRegister::X => {
                                    add_instruction!(set_m_short!());
                                    add_instruction!(W65C816::Instruction::PhyImplied);
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::PlyImplied);
                                    add_instruction!(set_m_long!());
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(set_m_short!());
                                    add_instruction!(
                                        W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(set_m_long!());
                                    add_instruction!(W65C816::Instruction::StaDirectPage(taddr));
                                }
                            },
                            CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                        }
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_store_byte_instruction(
    source: &StoreSource,
    target: &StoreTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match source {
        StoreSource::IndexRegister(si) => match si {
            IndexRegister::X => match target {
                StoreTarget::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            add_instruction!(set_m_short!());
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(
                                        comp_addr
                                    ));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(
                                        comp_addr
                                    ));
                                }
                            }
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            add_instruction!(set_m_short!());
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::PhyImplied);
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::PlyImplied);
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
            IndexRegister::Y => match target {
                StoreTarget::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            add_instruction!(set_m_short!());
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(
                                        comp_addr
                                    ));
                                }
                                IndexRegister::Y => {
                                    add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(
                                        comp_addr
                                    ));
                                }
                            }
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            add_instruction!(set_m_short!());
                            add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            add_instruction!(set_m_short!());
                            match index {
                                IndexRegister::X => {
                                    add_instruction!(W65C816::Instruction::TxyImplied);
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                    add_instruction!(W65C816::Instruction::TayImplied);
                                }
                                IndexRegister::Y => {
                                    add_instruction!(
                                        W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                    );
                                }
                            }
                            add_instruction!(set_m_long!());
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
        },
        StoreSource::GeneralPurposeRegister(sgp) => {
            let saddr_opt = get_register_address(sgp, &state.reg_defines);
            match saddr_opt {
                CompileResult::Success(saddr) => {
                    add_instruction!(set_m_short!());
                    add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                    match target {
                        StoreTarget::Absolute(addr) => {
                            match compile_expression(addr, identifiers, scope_info, state) {
                                CompileResult::Success(comp_addr) => {
                                    add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                                }
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                        StoreTarget::AbsoluteIndexed(addr, index) => {
                            match compile_expression(addr, identifiers, scope_info, state) {
                                CompileResult::Success(comp_addr) => match index {
                                    IndexRegister::X => {
                                        add_instruction!(
                                            W65C816::Instruction::StaAbsoluteIndexedX(comp_addr)
                                        );
                                    }
                                    IndexRegister::Y => {
                                        add_instruction!(
                                            W65C816::Instruction::StaAbsoluteIndexedY(comp_addr)
                                        );
                                    }
                                },
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                        StoreTarget::RegisterIndirect(reg) => {
                            let addr_opt = get_register_address(reg, &state.reg_defines);
                            match addr_opt {
                                CompileResult::Success(addr) => {
                                    add_instruction!(W65C816::Instruction::StaDirectPageIndirect(
                                        addr
                                    ));
                                }
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                        StoreTarget::RegisterIndirectIndexed(reg, index) => {
                            let addr_opt = get_register_address(reg, &state.reg_defines);
                            match addr_opt {
                                CompileResult::Success(addr) => match index {
                                    IndexRegister::X => {
                                        add_instruction!(W65C816::Instruction::PhyImplied);
                                        add_instruction!(W65C816::Instruction::TxyImplied);
                                        add_instruction!(
                                            W65C816::Instruction::StaDirectPageIndirectIndexedY(
                                                addr
                                            )
                                        );
                                        add_instruction!(W65C816::Instruction::PlyImplied);
                                    }
                                    IndexRegister::Y => {
                                        add_instruction!(
                                            W65C816::Instruction::StaDirectPageIndirectIndexedY(
                                                addr
                                            )
                                        );
                                    }
                                },
                                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                            }
                        }
                    }
                    add_instruction!(set_m_long!());
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        StoreSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(set_m_short!());
                    add_instruction!(W65C816::Instruction::LdaImmediate(comp_value, true));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }

            match target {
                StoreTarget::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(
                                    comp_addr
                                ));
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(
                                    comp_addr
                                ));
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                StoreTarget::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::PhyImplied);
                                add_instruction!(W65C816::Instruction::TxyImplied);
                                add_instruction!(
                                    W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                );
                                add_instruction!(W65C816::Instruction::PlyImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(
                                    W65C816::Instruction::StaDirectPageIndirectIndexedY(addr)
                                );
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            }
            add_instruction!(set_m_long!());
        }
    }

    CompileResult::Success(result)
}

fn compile_push_instruction(
    source: &StackArg,
    _identifiers: &IdentifierCollection,
    _scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match source {
        StackArg::IndexRegister(si) => match si {
            IndexRegister::X => {
                add_instruction!(W65C816::Instruction::PhxImplied);
            }
            IndexRegister::Y => {
                add_instruction!(W65C816::Instruction::PhyImplied);
            }
        },
        StackArg::GeneralPurposeRegister(sgp) => {
            let addr_opt = get_register_address(sgp, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPage(addr));
                    add_instruction!(W65C816::Instruction::PhaImplied);
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        StackArg::SpecialPurposeRegister(ssp) => match ssp {
            SpecialPurposeRegister::StackPointer => {
                add_instruction!(W65C816::Instruction::TscImplied);
                add_instruction!(W65C816::Instruction::PhaImplied);
            }
            SpecialPurposeRegister::StatusFlags => {
                add_instruction!(W65C816::Instruction::PhpImplied);
            }
            SpecialPurposeRegister::DataBank => {
                add_instruction!(W65C816::Instruction::PhbImplied);
            }
        },
    }

    CompileResult::Success(result)
}

fn compile_pop_instruction(
    target: &StackArg,
    _identifiers: &IdentifierCollection,
    _scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        StackArg::IndexRegister(ti) => match ti {
            IndexRegister::X => {
                add_instruction!(W65C816::Instruction::PlxImplied);
            }
            IndexRegister::Y => {
                add_instruction!(W65C816::Instruction::PlyImplied);
            }
        },
        StackArg::GeneralPurposeRegister(tgp) => {
            let addr_opt = get_register_address(tgp, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::PlaImplied);
                    add_instruction!(W65C816::Instruction::StaDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        StackArg::SpecialPurposeRegister(tsp) => match tsp {
            SpecialPurposeRegister::StackPointer => {
                add_instruction!(W65C816::Instruction::PlaImplied);
                add_instruction!(W65C816::Instruction::TcsImplied);
            }
            SpecialPurposeRegister::StatusFlags => {
                add_instruction!(W65C816::Instruction::PlpImplied);
            }
            SpecialPurposeRegister::DataBank => {
                add_instruction!(W65C816::Instruction::PlbImplied);
            }
        },
    }

    CompileResult::Success(result)
}

//match compile_expression(addr, identifiers, scope_info, state) {
//    CompileResult::Success(comp_addr) => {}
//    CompileResult::Failure(msg) => return CompileResult::Failure(msg),
//}

fn compile_increment_instruction(
    target: &CountTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        CountTarget::IndexRegister(ti) => match ti {
            IndexRegister::X => {
                add_instruction!(W65C816::Instruction::InxImplied);
            }
            IndexRegister::Y => {
                add_instruction!(W65C816::Instruction::InyImplied);
            }
        },
        CountTarget::GeneralPurposeRegister(tgp) => {
            let addr_opt = get_register_address(tgp, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::IncDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        CountTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::IncAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        CountTarget::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::IncAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TxaImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::IncAbsoluteIndexedX(comp_addr));
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_decrement_instruction(
    target: &CountTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        CountTarget::IndexRegister(ti) => match ti {
            IndexRegister::X => {
                add_instruction!(W65C816::Instruction::DexImplied);
            }
            IndexRegister::Y => {
                add_instruction!(W65C816::Instruction::DeyImplied);
            }
        },
        CountTarget::GeneralPurposeRegister(tgp) => {
            let addr_opt = get_register_address(tgp, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::DecDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        CountTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::DecAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        CountTarget::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::IncAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TxaImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::DecAbsoluteIndexedX(comp_addr));
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_left_alu_source(
    source: &AluSource,
    result: &mut Vec<W65C816::Statement>,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<()> {
    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match source {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::LdaImmediate(comp_value, false));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::LdaAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(())
}

fn compile_alu_target(
    target: &AluTarget,
    result: &mut Vec<W65C816::Statement>,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<()> {
    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        AluTarget::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::StaDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::StaAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluTarget::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::StaAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::StaAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluTarget::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::StaDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluTarget::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::StaDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::StaDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(())
}

fn compile_add_instruction(
    left: &AluSource,
    right: &AluSource,
    target: &AluTarget,
    with_carry: bool,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if !with_carry {
        add_instruction!(W65C816::Instruction::ClcImplied);
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(left, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    match right {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::AdcDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::AdcImmediate(comp_value));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::AdcAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::AdcAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::AdcAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::AdcDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::AdcDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::AdcDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_subtract_instruction(
    left: &AluSource,
    right: &AluSource,
    target: &AluTarget,
    with_borrow: bool,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if !with_borrow {
        add_instruction!(W65C816::Instruction::SecImplied);
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(left, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    match right {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::SbcDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::SbcImmediate(comp_value));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::SbcAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::SbcAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::SbcAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::SbcDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::SbcDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::SbcDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_bitwise_and_instruction(
    left: &AluSource,
    right: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(left, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    match right {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::AndDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::AndImmediate(comp_value));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::AndAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::AndAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::AndAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::AndDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::AndDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::AndDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_bitwise_xor_instruction(
    left: &AluSource,
    right: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(left, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    match right {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::EorDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::EorImmediate(comp_value));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::EorAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::EorAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::EorAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::EorDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::EorDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::EorDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_bitwise_or_instruction(
    left: &AluSource,
    right: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(left, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    match right {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::OraDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::OraImmediate(comp_value));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::OraAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::OraAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::OraAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::OraDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::OraDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::OraDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_negate_instruction(
    source: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    add_instruction!(W65C816::Instruction::LdaImmediate(ZERO.clone(), false));
    add_instruction!(W65C816::Instruction::SecImplied);

    match source {
        AluSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::SbcDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::SbcImmediate(comp_value));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::SbcAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::SbcAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::SbcAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::SbcDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        AluSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::SbcDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::SbcDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_bitwise_not_instruction(
    source: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(source, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    add_instruction!(W65C816::Instruction::EorImmediate(MAX_VAL.clone()));

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_shift_left_instruction(
    source: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(source, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    add_instruction!(W65C816::Instruction::AslAccumulator);

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_shift_right_instruction(
    source: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(source, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    add_instruction!(W65C816::Instruction::LsrAccumulator);

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_rotate_left_instruction(
    source: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(source, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    add_instruction!(W65C816::Instruction::RolAccumulator);

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_rotate_right_instruction(
    source: &AluSource,
    target: &AluTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    if let CompileResult::Failure(msg) =
        compile_left_alu_source(source, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    add_instruction!(W65C816::Instruction::RorAccumulator);

    if let CompileResult::Failure(msg) =
        compile_alu_target(target, &mut result, identifiers, scope_info, state)
    {
        return CompileResult::Failure(msg);
    }

    CompileResult::Success(result)
}

fn compile_conditional_jump_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
    get_branch_instruction: fn(&String) -> W65C816::Instruction,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    let label_name = format!("{}{:08X}", JUMP_LABEL_PREFIX, jump_label_count);
    *jump_label_count += 1;

    add_instruction!(get_branch_instruction(&label_name));

    match target {
        JumpTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmlAbsoluteLong(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndirect(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmpAbsoluteIndirect(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndexedIndirect(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TxaImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    result.push(W65C816::Statement::Label(label_name));

    CompileResult::Success(result)
}

fn compile_jump_if_equal_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BneRelative(label_name.clone()),
    )
}

fn compile_jump_if_not_equal_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BeqRelative(label_name.clone()),
    )
}

fn compile_jump_if_greater_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    let label_name = format!("{}{:08X}", JUMP_LABEL_PREFIX, jump_label_count);
    *jump_label_count += 1;

    add_instruction!(W65C816::Instruction::BmiRelative(label_name.clone()));
    add_instruction!(W65C816::Instruction::BeqRelative(label_name.clone()));

    match target {
        JumpTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmlAbsoluteLong(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndirect(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmpAbsoluteIndirect(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndexedIndirect(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TxaImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    result.push(W65C816::Statement::Label(label_name));

    CompileResult::Success(result)
}

fn compile_jump_if_less_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BplRelative(label_name.clone()),
    )
}

fn compile_jump_if_greater_or_equal_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BmiRelative(label_name.clone()),
    )
}

fn compile_jump_if_less_or_equal_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    let label_name_1 = format!("{}{:08X}", JUMP_LABEL_PREFIX, jump_label_count);
    *jump_label_count += 1;
    let label_name_2 = format!("{}{:08X}", JUMP_LABEL_PREFIX, jump_label_count);
    *jump_label_count += 1;

    add_instruction!(W65C816::Instruction::BeqRelative(label_name_1.clone()));
    add_instruction!(W65C816::Instruction::BplRelative(label_name_2.clone()));
    result.push(W65C816::Statement::Label(label_name_1));

    match target {
        JumpTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmlAbsoluteLong(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndirect(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmpAbsoluteIndirect(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndexedIndirect(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TxaImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    result.push(W65C816::Statement::Label(label_name_2));

    CompileResult::Success(result)
}

fn compile_jump_on_carry_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BccRelative(label_name.clone()),
    )
}

fn compile_jump_on_carry_clear_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BcsRelative(label_name.clone()),
    )
}

fn compile_jump_on_overflow_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BvcRelative(label_name.clone()),
    )
}

fn compile_jump_on_no_overflow_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    compile_conditional_jump_instruction(
        target,
        identifiers,
        scope_info,
        state,
        jump_label_count,
        |label_name| W65C816::Instruction::BvsRelative(label_name.clone()),
    )
}

fn compile_jump_instruction(
    target: &JumpTarget,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match target {
        JumpTarget::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmlAbsoluteLong(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndirect(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::JmpAbsoluteIndirect(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        JumpTarget::AbsoluteIndexedIndirect(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::TxaImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::JmpAbsoluteIndexedXIndirect(
                            comp_addr
                        ));
                        add_instruction!(W65C816::Instruction::TaxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_compare_instruction(
    left: &CompareSource,
    right: &CompareWithSource,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match left {
        CompareSource::IndexRegister(reg) => match reg {
            IndexRegister::X => match right {
                CompareWithSource::GeneralPurposeRegister(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::CpxDirectPage(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(W65C816::Instruction::CpxImmediate(comp_value));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::CpxAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::TxaImplied);
                                add_instruction!(W65C816::Instruction::CmpAbsoluteIndexedX(
                                    comp_addr
                                ));
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::TxaImplied);
                                add_instruction!(W65C816::Instruction::CmpAbsoluteIndexedY(
                                    comp_addr
                                ));
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TxaImplied);
                            add_instruction!(W65C816::Instruction::CmpDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::PhyImplied);
                                add_instruction!(W65C816::Instruction::TxyImplied);
                                add_instruction!(W65C816::Instruction::TxaImplied);
                                add_instruction!(
                                    W65C816::Instruction::CmpDirectPageIndirectIndexedY(addr)
                                );
                                add_instruction!(W65C816::Instruction::PlyImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::TxaImplied);
                                add_instruction!(
                                    W65C816::Instruction::CmpDirectPageIndirectIndexedY(addr)
                                );
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
            IndexRegister::Y => match right {
                CompareWithSource::GeneralPurposeRegister(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::CpyDirectPage(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(W65C816::Instruction::CpyImmediate(comp_value));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::CpyAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::TyaImplied);
                                add_instruction!(W65C816::Instruction::CmpAbsoluteIndexedX(
                                    comp_addr
                                ));
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::TyaImplied);
                                add_instruction!(W65C816::Instruction::CmpAbsoluteIndexedY(
                                    comp_addr
                                ));
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::TyaImplied);
                            add_instruction!(W65C816::Instruction::CmpDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::TyaImplied);
                                add_instruction!(W65C816::Instruction::TxyImplied);
                                add_instruction!(
                                    W65C816::Instruction::CmpDirectPageIndirectIndexedY(addr)
                                );
                                add_instruction!(W65C816::Instruction::TayImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::TyaImplied);
                                add_instruction!(
                                    W65C816::Instruction::CmpDirectPageIndirectIndexedY(addr)
                                );
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            },
        },
        CompareSource::GeneralPurposeRegister(reg) => {
            let saddr_opt = get_register_address(reg, &state.reg_defines);
            match saddr_opt {
                CompileResult::Success(saddr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPage(saddr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }

            match right {
                CompareWithSource::GeneralPurposeRegister(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::CmpDirectPage(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::Immediate(value) => {
                    match compile_expression(value, identifiers, scope_info, state) {
                        CompileResult::Success(comp_value) => {
                            add_instruction!(W65C816::Instruction::CmpImmediate(comp_value));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::Absolute(addr) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => {
                            add_instruction!(W65C816::Instruction::CmpAbsolute(comp_addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::AbsoluteIndexed(addr, index) => {
                    match compile_expression(addr, identifiers, scope_info, state) {
                        CompileResult::Success(comp_addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::CmpAbsoluteIndexedX(
                                    comp_addr
                                ));
                            }
                            IndexRegister::Y => {
                                add_instruction!(W65C816::Instruction::CmpAbsoluteIndexedY(
                                    comp_addr
                                ));
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::RegisterIndirect(reg) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => {
                            add_instruction!(W65C816::Instruction::CmpDirectPageIndirect(addr));
                        }
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
                CompareWithSource::RegisterIndirectIndexed(reg, index) => {
                    let addr_opt = get_register_address(reg, &state.reg_defines);
                    match addr_opt {
                        CompileResult::Success(addr) => match index {
                            IndexRegister::X => {
                                add_instruction!(W65C816::Instruction::PhyImplied);
                                add_instruction!(W65C816::Instruction::TxyImplied);
                                add_instruction!(
                                    W65C816::Instruction::CmpDirectPageIndirectIndexedY(addr)
                                );
                                add_instruction!(W65C816::Instruction::PlyImplied);
                            }
                            IndexRegister::Y => {
                                add_instruction!(
                                    W65C816::Instruction::CmpDirectPageIndirectIndexedY(addr)
                                );
                            }
                        },
                        CompileResult::Failure(msg) => return CompileResult::Failure(msg),
                    }
                }
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_status_flag_instruction(
    flags: &StatusFlagsSource,
    set: bool,
    _identifiers: &IdentifierCollection,
    _scope_info: Option<(&str, &BlockIdentifierCollection)>,
    _state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    let value = flags.get_value();
    let expr = Expression::Literal(Wrapping(value as isize));
    if expr == C_FLAG {
        if set {
            add_instruction!(W65C816::Instruction::SecImplied);
        } else {
            add_instruction!(W65C816::Instruction::ClcImplied);
        }
    } else if expr == D_FLAG {
        if set {
            add_instruction!(W65C816::Instruction::SedImplied);
        } else {
            add_instruction!(W65C816::Instruction::CldImplied);
        }
    } else if expr == I_FLAG {
        if set {
            add_instruction!(W65C816::Instruction::SeiImplied);
        } else {
            add_instruction!(W65C816::Instruction::CliImplied);
        }
    } else if expr == O_FLAG {
        if set {
            add_instruction!(W65C816::Instruction::SepImmediate(O_FLAG.clone()));
        } else {
            add_instruction!(W65C816::Instruction::ClvImplied);
        }
    } else {
        if set {
            add_instruction!(W65C816::Instruction::SepImmediate(expr));
        } else {
            add_instruction!(W65C816::Instruction::RepImmediate(expr));
        }
    }

    CompileResult::Success(result)
}

fn compile_bit_test_instruction(
    left: &BitTestSource,
    right: &BitTestWithSource,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();

    macro_rules! add_instruction {
        ($inst:expr) => {
            result.push(W65C816::Statement::Instruction($inst));
        };
    }

    match right {
        BitTestWithSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestWithSource::Immediate(value) => {
            match compile_expression(value, identifiers, scope_info, state) {
                CompileResult::Success(comp_value) => {
                    add_instruction!(W65C816::Instruction::LdaImmediate(comp_value, false));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestWithSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::LdaAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestWithSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::LdaAbsoluteIndexedY(comp_addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestWithSource::RegisterIndirect(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::LdaDirectPageIndirect(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestWithSource::RegisterIndirectIndexed(reg, index) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::PhyImplied);
                        add_instruction!(W65C816::Instruction::TxyImplied);
                        add_instruction!(W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr));
                        add_instruction!(W65C816::Instruction::PlyImplied);
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::LdaDirectPageIndirectIndexedY(addr));
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    match left {
        BitTestSource::GeneralPurposeRegister(reg) => {
            let addr_opt = get_register_address(reg, &state.reg_defines);
            match addr_opt {
                CompileResult::Success(addr) => {
                    add_instruction!(W65C816::Instruction::BitDirectPage(addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestSource::Absolute(addr) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => {
                    add_instruction!(W65C816::Instruction::BitAbsolute(comp_addr));
                }
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
        BitTestSource::AbsoluteIndexed(addr, index) => {
            match compile_expression(addr, identifiers, scope_info, state) {
                CompileResult::Success(comp_addr) => match index {
                    IndexRegister::X => {
                        add_instruction!(W65C816::Instruction::BitAbsoluteIndexedX(comp_addr));
                    }
                    IndexRegister::Y => {
                        add_instruction!(W65C816::Instruction::PhxImplied);
                        add_instruction!(W65C816::Instruction::TyxImplied);
                        add_instruction!(W65C816::Instruction::BitAbsoluteIndexedX(comp_addr));
                        add_instruction!(W65C816::Instruction::PlxImplied);
                    }
                },
                CompileResult::Failure(msg) => return CompileResult::Failure(msg),
            }
        }
    }

    CompileResult::Success(result)
}

fn compile_instruction(
    inst: &Instruction,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &mut CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    match inst {
        Instruction::Transfer(s, t) => {
            compile_transfer_instruction(s, t, identifiers, scope_info, state)
        }
        Instruction::LoadWord(t, s) => {
            compile_load_word_instruction(t, s, identifiers, scope_info, state)
        }
        Instruction::StoreWord(s, t) => {
            compile_store_word_instruction(s, t, identifiers, scope_info, state)
        }
        Instruction::LoadByte(t, s) => {
            compile_load_byte_instruction(t, s, identifiers, scope_info, state)
        }
        Instruction::StoreByte(s, t) => {
            compile_store_byte_instruction(s, t, identifiers, scope_info, state)
        }
        Instruction::Push(s) => compile_push_instruction(s, identifiers, scope_info, state),
        Instruction::Pop(t) => compile_pop_instruction(t, identifiers, scope_info, state),
        Instruction::Increment(t) => {
            compile_increment_instruction(t, identifiers, scope_info, state)
        }
        Instruction::Decrement(t) => {
            compile_decrement_instruction(t, identifiers, scope_info, state)
        }
        Instruction::Add(left, right, target) => {
            compile_add_instruction(left, right, target, false, identifiers, scope_info, state)
        }
        Instruction::AddWithCarry(left, right, target) => {
            compile_add_instruction(left, right, target, true, identifiers, scope_info, state)
        }
        Instruction::Subtract(left, right, target) => {
            compile_subtract_instruction(left, right, target, false, identifiers, scope_info, state)
        }
        Instruction::SubtractWithBorrow(left, right, target) => {
            compile_subtract_instruction(left, right, target, true, identifiers, scope_info, state)
        }
        Instruction::BitwiseAnd(left, right, target) => {
            compile_bitwise_and_instruction(left, right, target, identifiers, scope_info, state)
        }
        Instruction::BitwiseXor(left, right, target) => {
            compile_bitwise_xor_instruction(left, right, target, identifiers, scope_info, state)
        }
        Instruction::BitwiseOr(left, right, target) => {
            compile_bitwise_or_instruction(left, right, target, identifiers, scope_info, state)
        }
        Instruction::Negate(source, target) => {
            compile_negate_instruction(source, target, identifiers, scope_info, state)
        }
        Instruction::BitwiseNot(source, target) => {
            compile_bitwise_not_instruction(source, target, identifiers, scope_info, state)
        }
        Instruction::ShiftLeft(source, target) => {
            compile_shift_left_instruction(source, target, identifiers, scope_info, state)
        }
        Instruction::ShiftRight(source, target) => {
            compile_shift_right_instruction(source, target, identifiers, scope_info, state)
        }
        Instruction::RotateLeft(source, target) => {
            compile_rotate_left_instruction(source, target, identifiers, scope_info, state)
        }
        Instruction::RotateRight(source, target) => {
            compile_rotate_right_instruction(source, target, identifiers, scope_info, state)
        }
        Instruction::JumpIfEqual(target) => compile_jump_if_equal_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpIfNotEqual(target) => compile_jump_if_not_equal_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpIfGreater(target) => compile_jump_if_greater_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpIfLess(target) => compile_jump_if_less_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpIfGreaterOrEqual(target) => compile_jump_if_greater_or_equal_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpIfLessOrEqual(target) => compile_jump_if_less_or_equal_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpOnCarry(target) => compile_jump_on_carry_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpOnCarryClear(target) => compile_jump_on_carry_clear_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpOnOverflow(target) => compile_jump_on_overflow_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::JumpOnNoOverflow(target) => compile_jump_on_no_overflow_instruction(
            target,
            identifiers,
            scope_info,
            state,
            jump_label_count,
        ),
        Instruction::Jump(target) => {
            compile_jump_instruction(target, identifiers, scope_info, state)
        }
        Instruction::Compare(left, right) => {
            compile_compare_instruction(left, right, identifiers, scope_info, state)
        }
        Instruction::SetStatusFlags(flags) => {
            compile_status_flag_instruction(flags, true, identifiers, scope_info, state)
        }
        Instruction::ResetStatusFlags(flags) => {
            compile_status_flag_instruction(flags, false, identifiers, scope_info, state)
        }
        Instruction::BitTest(left, right) => {
            compile_bit_test_instruction(left, right, identifiers, scope_info, state)
        }
        Instruction::SoftwareInterrupt => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            result.push(W65C816::Statement::Instruction(
                W65C816::Instruction::BrkImplied,
            ));
            CompileResult::Success(result)
        }
        Instruction::WaitForInterrupt => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            result.push(W65C816::Statement::Instruction(
                W65C816::Instruction::WaiImplied,
            ));
            CompileResult::Success(result)
        }
    }
}

fn compile_directive(
    dir: &Directive,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &mut CompileState,
) -> CompileResult<Vec<W65C816::Statement>> {
    match dir {
        Directive::Origin(expr) => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            match compile_expression(expr, identifiers, scope_info, state) {
                CompileResult::Success(comp_expr) => {
                    result.push(W65C816::Statement::Directive(W65C816::Directive::Origin(
                        comp_expr,
                    )));
                    CompileResult::Success(result)
                }
                CompileResult::Failure(msg) => CompileResult::Failure(msg),
            }
        }
        Directive::Define(name, expr) => {
            match compile_expression(expr, identifiers, scope_info, state) {
                CompileResult::Success(comp_expr) => {
                    state.defines.insert(name.clone(), comp_expr);
                    CompileResult::Success(Vec::new())
                }
                CompileResult::Failure(msg) => CompileResult::Failure(msg),
            }
        }
        Directive::DefineRegister(name, reg) => {
            state.reg_defines.insert(name.clone(), reg.clone());
            CompileResult::Success(Vec::new())
        }
        Directive::StoreByte(exprs) => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            match compile_expression_list(&exprs.0, identifiers, scope_info, state) {
                CompileResult::Success(comp_exprs) => {
                    result.push(W65C816::Statement::Directive(
                        W65C816::Directive::StoreByte(DisplayableVec(comp_exprs)),
                    ));
                    CompileResult::Success(result)
                }
                CompileResult::Failure(msg) => CompileResult::Failure(msg),
            }
        }
        Directive::StoreWord(exprs) => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            match compile_expression_list(&exprs.0, identifiers, scope_info, state) {
                CompileResult::Success(comp_exprs) => {
                    result.push(W65C816::Statement::Directive(
                        W65C816::Directive::StoreWord(DisplayableVec(comp_exprs)),
                    ));
                    CompileResult::Success(result)
                }
                CompileResult::Failure(msg) => CompileResult::Failure(msg),
            }
        }
        Directive::AsciiString(s, null_terminated) => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            result.push(W65C816::Statement::Directive(
                W65C816::Directive::AsciiString(s.to_string(), *null_terminated),
            ));
            CompileResult::Success(result)
        }
        Directive::UnicodeString(s, null_terminated) => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            result.push(W65C816::Statement::Directive(
                W65C816::Directive::UnicodeString(s.to_string(), *null_terminated),
            ));
            CompileResult::Success(result)
        }
        Directive::Call(name, params) => {
            if identifiers.contains(name) {
                match identifiers.functions.get(name) {
                    Some((param_count, _)) => {
                        if params.0.len() == *param_count {
                            let mut result: Vec<W65C816::Statement> = Vec::new();

                            for (i, p) in params.0.iter().enumerate() {
                                let taddr = Expression::Literal(Wrapping(i as isize * 2));
                                match p {
                                    Parameter::IndexRegister(reg) => match reg {
                                        IndexRegister::X => {
                                            result.push(W65C816::Statement::Instruction(
                                                W65C816::Instruction::StxDirectPage(taddr),
                                            ));
                                        }
                                        IndexRegister::Y => {
                                            result.push(W65C816::Statement::Instruction(
                                                W65C816::Instruction::StyDirectPage(taddr),
                                            ));
                                        }
                                    },
                                    Parameter::GeneralPurposeRegister(reg) => {
                                        let saddr_opt =
                                            get_register_address(reg, &state.reg_defines);
                                        match saddr_opt {
                                            CompileResult::Success(saddr) => {
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::LdaDirectPage(saddr),
                                                ));
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::StaDirectPage(taddr),
                                                ));
                                            }
                                            CompileResult::Failure(msg) => {
                                                return CompileResult::Failure(msg)
                                            }
                                        }
                                    }
                                    Parameter::Immediate(value) => {
                                        match compile_expression(
                                            value,
                                            identifiers,
                                            scope_info,
                                            state,
                                        ) {
                                            CompileResult::Success(comp_value) => {
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::LdaImmediate(
                                                        comp_value, false,
                                                    ),
                                                ));
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::StaDirectPage(taddr),
                                                ));
                                            }
                                            CompileResult::Failure(msg) => {
                                                return CompileResult::Failure(msg)
                                            }
                                        }
                                    }
                                    Parameter::Absolute(addr) => {
                                        match compile_expression(
                                            addr,
                                            identifiers,
                                            scope_info,
                                            state,
                                        ) {
                                            CompileResult::Success(comp_addr) => {
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::LdaAbsolute(comp_addr),
                                                ));
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::StaDirectPage(taddr),
                                                ));
                                            }
                                            CompileResult::Failure(msg) => {
                                                return CompileResult::Failure(msg)
                                            }
                                        }
                                    }
                                    Parameter::AbsoluteIndexed(addr, index) => {
                                        match compile_expression(
                                            addr,
                                            identifiers,
                                            scope_info,
                                            state,
                                        ) {
                                            CompileResult::Success(comp_addr) => {
                                                match index {
                                                    IndexRegister::X => {
                                                        result.push(W65C816::Statement::Instruction(
                                                            W65C816::Instruction::LdaAbsoluteIndexedX(comp_addr),
                                                        ));
                                                    }
                                                    IndexRegister::Y => {
                                                        result.push(W65C816::Statement::Instruction(
                                                            W65C816::Instruction::LdaAbsoluteIndexedY(comp_addr),
                                                        ));
                                                    }
                                                }
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::StaDirectPage(taddr),
                                                ));
                                            }
                                            CompileResult::Failure(msg) => {
                                                return CompileResult::Failure(msg)
                                            }
                                        }
                                    }
                                    Parameter::RegisterIndirect(reg) => {
                                        let saddr_opt =
                                            get_register_address(reg, &state.reg_defines);
                                        match saddr_opt {
                                            CompileResult::Success(saddr) => {
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::LdaDirectPageIndirect(
                                                        saddr,
                                                    ),
                                                ));
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::StaDirectPage(taddr),
                                                ));
                                            }
                                            CompileResult::Failure(msg) => {
                                                return CompileResult::Failure(msg)
                                            }
                                        }
                                    }
                                    Parameter::RegisterIndirectIndexed(reg, index) => {
                                        let saddr_opt =
                                            get_register_address(reg, &state.reg_defines);
                                        match saddr_opt {
                                            CompileResult::Success(saddr) => {
                                                match index {
                                                    IndexRegister::X => {
                                                        result.push(
                                                            W65C816::Statement::Instruction(
                                                                W65C816::Instruction::PhyImplied,
                                                            ),
                                                        );
                                                        result.push(
                                                            W65C816::Statement::Instruction(
                                                                W65C816::Instruction::TxyImplied,
                                                            ),
                                                        );
                                                        result.push(W65C816::Statement::Instruction(
                                                            W65C816::Instruction::LdaDirectPageIndirectIndexedY(saddr),
                                                        ));
                                                        result.push(
                                                            W65C816::Statement::Instruction(
                                                                W65C816::Instruction::PlyImplied,
                                                            ),
                                                        );
                                                    }
                                                    IndexRegister::Y => {
                                                        result.push(W65C816::Statement::Instruction(
                                                            W65C816::Instruction::LdaDirectPageIndirectIndexedY(saddr),
                                                        ));
                                                    }
                                                }
                                                result.push(W65C816::Statement::Instruction(
                                                    W65C816::Instruction::StaDirectPage(taddr),
                                                ));
                                            }
                                            CompileResult::Failure(msg) => {
                                                return CompileResult::Failure(msg)
                                            }
                                        }
                                    }
                                }
                            }

                            result.push(W65C816::Statement::Instruction(
                                W65C816::Instruction::JslAbsoluteLong(Expression::Label(format!(
                                    "{}{}",
                                    FUNCTION_PREFIX, name
                                ))),
                            ));
                            CompileResult::Success(result)
                        } else {
                            CompileResult::Failure(format!(
                                "Incorrect number of arguments for function '{}'",
                                name
                            ))
                        }
                    }
                    None => CompileResult::Failure(format!("'{}' is not a function", name)),
                }
            } else {
                CompileResult::Failure(format!("'{}' is not defined", name))
            }
        }
        Directive::Return => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            result.push(W65C816::Statement::Instruction(
                W65C816::Instruction::RtlImplied,
            ));
            CompileResult::Success(result)
        }
    }
}

fn compile_statement(
    stm: &Statement,
    identifiers: &IdentifierCollection,
    scope_info: Option<(&str, &BlockIdentifierCollection)>,
    state: &mut CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    match stm {
        Statement::Instruction(inst) => {
            compile_instruction(inst, identifiers, scope_info, state, jump_label_count)
        }
        Statement::Directive(dir) => compile_directive(dir, identifiers, scope_info, state),
        Statement::Label(label) => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            match scope_info {
                Some((scope_name, _)) => {
                    result.push(W65C816::Statement::Label(format!(
                        "{}{}_{}",
                        SCOPE_LABEL_PREFIX, scope_name, label
                    )));
                }
                None => {
                    result.push(W65C816::Statement::Label(label.to_string()));
                }
            }
            CompileResult::Success(result)
        }
        Statement::Empty => {
            let mut result: Vec<W65C816::Statement> = Vec::with_capacity(1);
            result.push(W65C816::Statement::Empty);
            CompileResult::Success(result)
        }
    }
}

fn compile_function(
    name: &str,
    params: &DisplayableVec<String>,
    body: &Vec<Statement>,
    identifiers: &IdentifierCollection,
    block_identifiers: &BlockIdentifierCollection,
    mut state: CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();
    result.push(W65C816::Statement::Label(format!(
        "{}{}",
        FUNCTION_PREFIX, name
    )));

    for (i, p) in params.0.iter().enumerate() {
        state
            .reg_defines
            .insert(p.clone(), GeneralPurposeRegister::Argument(i));
    }

    for stm in body.iter() {
        match compile_statement(
            stm,
            identifiers,
            Some((name, block_identifiers)),
            &mut state,
            jump_label_count,
        ) {
            CompileResult::Success(mut stms) => {
                result.append(&mut stms);
            }
            CompileResult::Failure(msg) => {
                return CompileResult::Failure(msg);
            }
        }
    }

    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::RtlImplied,
    ));
    CompileResult::Success(result)
}

fn compile_main(
    body: &Vec<Statement>,
    identifiers: &IdentifierCollection,
    mut state: CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();
    result.push(W65C816::Statement::Label(MAIN_FUNCTION_LABEL.to_string()));

    // initialization code
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::SeiImplied,
    ));
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::CldImplied,
    ));
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::ClcImplied,
    ));
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::XceImplied,
    ));
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::RepImmediate(Expression::BinaryOperator(
            BinaryOperatorType::BitwiseOr,
            Box::new(M_FLAG.clone()),
            Box::new(X_FLAG.clone()),
        )),
    ));
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::CliImplied,
    ));
    result.push(W65C816::Statement::Empty);

    for stm in body.iter() {
        match compile_statement(
            stm,
            identifiers,
            Some((
                MAIN_FUNCTION_LABEL,
                identifiers.main_entry_point.as_ref().unwrap(),
            )),
            &mut state,
            jump_label_count,
        ) {
            CompileResult::Success(mut stms) => {
                result.append(&mut stms);
            }
            CompileResult::Failure(msg) => {
                return CompileResult::Failure(msg);
            }
        }
    }

    result.push(W65C816::Statement::Empty);
    result.push(W65C816::Statement::Label(MAIN_LOOP_LABEL.to_string()));
    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::BraRelative(MAIN_LOOP_LABEL.to_string()),
    ));
    CompileResult::Success(result)
}

fn compile_irq(
    body: &Vec<Statement>,
    identifiers: &IdentifierCollection,
    mut state: CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();
    result.push(W65C816::Statement::Label(IRQ_FUNCTION_LABEL.to_string()));

    for stm in body.iter() {
        match compile_statement(
            stm,
            identifiers,
            Some((
                IRQ_FUNCTION_LABEL,
                identifiers.irq_entry_point.as_ref().unwrap(),
            )),
            &mut state,
            jump_label_count,
        ) {
            CompileResult::Success(mut stms) => {
                result.append(&mut stms);
            }
            CompileResult::Failure(msg) => {
                return CompileResult::Failure(msg);
            }
        }
    }

    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::RtiImplied,
    ));
    CompileResult::Success(result)
}

fn compile_nmi(
    body: &Vec<Statement>,
    identifiers: &IdentifierCollection,
    mut state: CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();
    result.push(W65C816::Statement::Label(NMI_FUNCTION_LABEL.to_string()));

    for stm in body.iter() {
        match compile_statement(
            stm,
            identifiers,
            Some((
                NMI_FUNCTION_LABEL,
                identifiers.nmi_entry_point.as_ref().unwrap(),
            )),
            &mut state,
            jump_label_count,
        ) {
            CompileResult::Success(mut stms) => {
                result.append(&mut stms);
            }
            CompileResult::Failure(msg) => {
                return CompileResult::Failure(msg);
            }
        }
    }

    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::RtiImplied,
    ));
    CompileResult::Success(result)
}

fn compile_brk(
    body: &Vec<Statement>,
    identifiers: &IdentifierCollection,
    mut state: CompileState,
    jump_label_count: &mut u32,
) -> CompileResult<Vec<W65C816::Statement>> {
    let mut result: Vec<W65C816::Statement> = Vec::new();
    result.push(W65C816::Statement::Label(BRK_FUNCTION_LABEL.to_string()));

    for stm in body.iter() {
        match compile_statement(
            stm,
            identifiers,
            Some((
                BRK_FUNCTION_LABEL,
                identifiers.brk_entry_point.as_ref().unwrap(),
            )),
            &mut state,
            jump_label_count,
        ) {
            CompileResult::Success(mut stms) => {
                result.append(&mut stms);
            }
            CompileResult::Failure(msg) => {
                return CompileResult::Failure(msg);
            }
        }
    }

    result.push(W65C816::Statement::Instruction(
        W65C816::Instruction::RtiImplied,
    ));
    CompileResult::Success(result)
}

pub fn compile_program(prog: &T816::Program) -> CompileResult<W65C816::Program> {
    match preprocess_identifiers(prog) {
        CompileResult::Success(identifiers) => {
            let mut result: Vec<W65C816::Statement> = Vec::new();

            let mut state = CompileState::new();
            let mut jump_label_count: u32 = 0;

            for tlstm in prog.body.iter() {
                match tlstm {
                    TopLevelStatement::Statement(stm) => {
                        match compile_statement(
                            stm,
                            &identifiers,
                            None,
                            &mut state,
                            &mut jump_label_count,
                        ) {
                            CompileResult::Success(mut stms) => {
                                result.append(&mut stms);
                            }
                            CompileResult::Failure(msg) => {
                                return CompileResult::Failure(msg);
                            }
                        }
                    }
                    TopLevelStatement::Function(name, params, body) => {
                        match compile_function(
                            name,
                            params,
                            body,
                            &identifiers,
                            &identifiers.functions.get(name).unwrap().1,
                            state.clone(),
                            &mut jump_label_count,
                        ) {
                            CompileResult::Success(mut stms) => {
                                result.append(&mut stms);
                            }
                            CompileResult::Failure(msg) => {
                                return CompileResult::Failure(msg);
                            }
                        }
                    }
                    TopLevelStatement::MainEntryPoint(body) => {
                        match compile_main(body, &identifiers, state.clone(), &mut jump_label_count)
                        {
                            CompileResult::Success(mut stms) => {
                                result.append(&mut stms);
                            }
                            CompileResult::Failure(msg) => {
                                return CompileResult::Failure(msg);
                            }
                        }
                    }
                    TopLevelStatement::IrqEntryPoint(body) => {
                        match compile_irq(body, &identifiers, state.clone(), &mut jump_label_count)
                        {
                            CompileResult::Success(mut stms) => {
                                result.append(&mut stms);
                            }
                            CompileResult::Failure(msg) => {
                                return CompileResult::Failure(msg);
                            }
                        }
                    }
                    TopLevelStatement::NmiEntryPoint(body) => {
                        match compile_nmi(body, &identifiers, state.clone(), &mut jump_label_count)
                        {
                            CompileResult::Success(mut stms) => {
                                result.append(&mut stms);
                            }
                            CompileResult::Failure(msg) => {
                                return CompileResult::Failure(msg);
                            }
                        }
                    }
                    TopLevelStatement::BrkEntryPoint(body) => {
                        match compile_brk(body, &identifiers, state.clone(), &mut jump_label_count)
                        {
                            CompileResult::Success(mut stms) => {
                                result.append(&mut stms);
                            }
                            CompileResult::Failure(msg) => {
                                return CompileResult::Failure(msg);
                            }
                        }
                    }
                }
            }

            // compiler functions
            result.push(W65C816::Statement::Empty);
            result.push(W65C816::Statement::Directive(W65C816::Directive::Origin(
                COMPILER_FUNCTIONS_OFFSET.clone(),
            )));
            result.push(W65C816::Statement::Empty);
            result.push(W65C816::Statement::Label(EMPTY_INTERRUPT_LABEL.to_string()));
            if let None = identifiers.irq_entry_point {
                result.push(W65C816::Statement::Label(IRQ_FUNCTION_LABEL.to_string()));
            }
            if let None = identifiers.nmi_entry_point {
                result.push(W65C816::Statement::Label(NMI_FUNCTION_LABEL.to_string()));
            }
            if let None = identifiers.brk_entry_point {
                result.push(W65C816::Statement::Label(BRK_FUNCTION_LABEL.to_string()));
            }
            result.push(W65C816::Statement::Instruction(
                W65C816::Instruction::RtiImplied,
            ));

            if let Some(_) = identifiers.main_entry_point {
                // vectors
                result.push(W65C816::Statement::Empty);
                result.push(W65C816::Statement::Directive(W65C816::Directive::Origin(
                    VECTOR_OFFSET.clone(),
                )));
                result.push(W65C816::Statement::Directive(
                    W65C816::Directive::StoreWord(DisplayableVec(vec![
                        ZERO.clone(),
                        ZERO.clone(),
                        ZERO.clone(),
                        Expression::Label(BRK_FUNCTION_LABEL.to_string()),
                        Expression::Label(EMPTY_INTERRUPT_LABEL.to_string()),
                        Expression::Label(NMI_FUNCTION_LABEL.to_string()),
                        ZERO.clone(),
                        Expression::Label(IRQ_FUNCTION_LABEL.to_string()),
                        ZERO.clone(),
                        ZERO.clone(),
                        ZERO.clone(),
                        ZERO.clone(),
                        Expression::Label(EMPTY_INTERRUPT_LABEL.to_string()),
                        Expression::Label(EMPTY_INTERRUPT_LABEL.to_string()),
                        Expression::Label(MAIN_FUNCTION_LABEL.to_string()),
                        Expression::Label(EMPTY_INTERRUPT_LABEL.to_string()),
                    ])),
                ));
            }

            CompileResult::Success(W65C816::Program(result))
        }
        CompileResult::Failure(msg) => CompileResult::Failure(msg),
    }
}
