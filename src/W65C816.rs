use crate::*;
use std::fmt::Display;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    AdcImmediate(Expression),
    AdcAbsolute(Expression),
    AdcAbsoluteLong(Expression),
    AdcDirectPage(Expression),
    AdcDirectPageIndirect(Expression),
    AdcDirectPageIndirectLong(Expression),
    AdcAbsoluteIndexedX(Expression),
    AdcAbsoluteLongIndexedX(Expression),
    AdcAbsoluteIndexedY(Expression),
    AdcDirectPageIndexedX(Expression),
    AdcDirectPageIndexedXIndirect(Expression),
    AdcDirectPageIndirectIndexedY(Expression),
    AdcDirectPageIndirectLongIndexedY(Expression),
    AdcStackRelative(Expression),
    AdcStackRelativeIndirectIndexedY(Expression),

    AndImmediate(Expression),
    AndAbsolute(Expression),
    AndAbsoluteLong(Expression),
    AndDirectPage(Expression),
    AndDirectPageIndirect(Expression),
    AndDirectPageIndirectLong(Expression),
    AndAbsoluteIndexedX(Expression),
    AndAbsoluteLongIndexedX(Expression),
    AndAbsoluteIndexedY(Expression),
    AndDirectPageIndexedX(Expression),
    AndDirectPageIndexedXIndirect(Expression),
    AndDirectPageIndirectIndexedY(Expression),
    AndDirectPageIndirectLongIndexedY(Expression),
    AndStackRelative(Expression),
    AndStackRelativeIndirectIndexedY(Expression),

    AslAccumulator,
    AslAbsolute(Expression),
    AslDirectPage(Expression),
    AslAbsoluteIndexedX(Expression),
    AslDirectPageIndexedX(Expression),

    BccRelative(String),
    BcsRelative(String),
    BeqRelative(String),
    BneRelative(String),
    BmiRelative(String),
    BplRelative(String),
    BvcRelative(String),
    BvsRelative(String),
    BraRelative(String),
    BrlRelative(String),

    BitImmediate(Expression),
    BitAbsolute(Expression),
    BitDirectPage(Expression),
    BitAbsoluteIndexedX(Expression),
    BitDirectPageIndexedX(Expression),

    BrkImplied,
    CopImplied,

    ClcImplied,
    CliImplied,
    CldImplied,
    ClvImplied,

    CmpImmediate(Expression),
    CmpAbsolute(Expression),
    CmpAbsoluteLong(Expression),
    CmpDirectPage(Expression),
    CmpDirectPageIndirect(Expression),
    CmpDirectPageIndirectLong(Expression),
    CmpAbsoluteIndexedX(Expression),
    CmpAbsoluteLongIndexedX(Expression),
    CmpAbsoluteIndexedY(Expression),
    CmpDirectPageIndexedX(Expression),
    CmpDirectPageIndexedXIndirect(Expression),
    CmpDirectPageIndirectIndexedY(Expression),
    CmpDirectPageIndirectLongIndexedY(Expression),
    CmpStackRelative(Expression),
    CmpStackRelativeIndirectIndexedY(Expression),

    CpxImmediate(Expression),
    CpxAbsolute(Expression),
    CpxDirectPage(Expression),

    CpyImmediate(Expression),
    CpyAbsolute(Expression),
    CpyDirectPage(Expression),

    DecAccumulator,
    DecAbsolute(Expression),
    DecDirectPage(Expression),
    DecAbsoluteIndexedX(Expression),
    DecDirectPageIndexedX(Expression),

    DexImplied,
    DeyImplied,

    EorImmediate(Expression),
    EorAbsolute(Expression),
    EorAbsoluteLong(Expression),
    EorDirectPage(Expression),
    EorDirectPageIndirect(Expression),
    EorDirectPageIndirectLong(Expression),
    EorAbsoluteIndexedX(Expression),
    EorAbsoluteLongIndexedX(Expression),
    EorAbsoluteIndexedY(Expression),
    EorDirectPageIndexedX(Expression),
    EorDirectPageIndexedXIndirect(Expression),
    EorDirectPageIndirectIndexedY(Expression),
    EorDirectPageIndirectLongIndexedY(Expression),
    EorStackRelative(Expression),
    EorStackRelativeIndirectIndexedY(Expression),

    IncAccumulator,
    IncAbsolute(Expression),
    IncDirectPage(Expression),
    IncAbsoluteIndexedX(Expression),
    IncDirectPageIndexedX(Expression),

    InxImplied,
    InyImplied,

    JmpAbsolute(Expression),
    JmpAbsoluteIndirect(Expression),
    JmpAbsoluteIndexedXIndirect(Expression),
    JmlAbsoluteLong(Expression),
    JmlAbsoluteIndirectLong(Expression),

    JsrAbsolute(Expression),
    JsrAbsoluteIndexedXIndirect(Expression),
    JslAbsoluteLong(Expression),

    LdaImmediate(Expression, bool),
    LdaAbsolute(Expression),
    LdaAbsoluteLong(Expression),
    LdaDirectPage(Expression),
    LdaDirectPageIndirect(Expression),
    LdaDirectPageIndirectLong(Expression),
    LdaAbsoluteIndexedX(Expression),
    LdaAbsoluteLongIndexedX(Expression),
    LdaAbsoluteIndexedY(Expression),
    LdaDirectPageIndexedX(Expression),
    LdaDirectPageIndexedXIndirect(Expression),
    LdaDirectPageIndirectIndexedY(Expression),
    LdaDirectPageIndirectLongIndexedY(Expression),
    LdaStackRelative(Expression),
    LdaStackRelativeIndirectIndexedY(Expression),

    LdxImmediate(Expression),
    LdxAbsolute(Expression),
    LdxDirectPage(Expression),
    LdxAbsoluteIndexedY(Expression),
    LdxDirectPageIndexedY(Expression),

    LdyImmediate(Expression),
    LdyAbsolute(Expression),
    LdyDirectPage(Expression),
    LdyAbsoluteIndexedX(Expression),
    LdyDirectPageIndexedX(Expression),

    LsrAccumulator,
    LsrAbsolute(Expression),
    LsrDirectPage(Expression),
    LsrAbsoluteIndexedX(Expression),
    LsrDirectPageIndexedX(Expression),

    MvnBlockMove(Expression, Expression),
    MvpBlockMove(Expression, Expression),

    NopImplied,

    OraImmediate(Expression),
    OraAbsolute(Expression),
    OraAbsoluteLong(Expression),
    OraDirectPage(Expression),
    OraDirectPageIndirect(Expression),
    OraDirectPageIndirectLong(Expression),
    OraAbsoluteIndexedX(Expression),
    OraAbsoluteLongIndexedX(Expression),
    OraAbsoluteIndexedY(Expression),
    OraDirectPageIndexedX(Expression),
    OraDirectPageIndexedXIndirect(Expression),
    OraDirectPageIndirectIndexedY(Expression),
    OraDirectPageIndirectLongIndexedY(Expression),
    OraStackRelative(Expression),
    OraStackRelativeIndirectIndexedY(Expression),

    PeaAbsolute(Expression),
    PeiDirectPageIndirect(Expression),
    PerRelativeLong(String),

    PhaImplied,
    PhbImplied,
    PhdImplied,
    PhkImplied,
    PhpImplied,
    PhxImplied,
    PhyImplied,

    PlaImplied,
    PlbImplied,
    PldImplied,
    PlpImplied,
    PlxImplied,
    PlyImplied,

    RepImmediate(Expression),

    RolAccumulator,
    RolAbsolute(Expression),
    RolDirectPage(Expression),
    RolAbsoluteIndexedX(Expression),
    RolDirectPageIndexedX(Expression),

    RorAccumulator,
    RorAbsolute(Expression),
    RorDirectPage(Expression),
    RorAbsoluteIndexedX(Expression),
    RorDirectPageIndexedX(Expression),

    RtiImplied,
    RtsImplied,
    RtlImplied,

    SbcImmediate(Expression),
    SbcAbsolute(Expression),
    SbcAbsoluteLong(Expression),
    SbcDirectPage(Expression),
    SbcDirectPageIndirect(Expression),
    SbcDirectPageIndirectLong(Expression),
    SbcAbsoluteIndexedX(Expression),
    SbcAbsoluteLongIndexedX(Expression),
    SbcAbsoluteIndexedY(Expression),
    SbcDirectPageIndexedX(Expression),
    SbcDirectPageIndexedXIndirect(Expression),
    SbcDirectPageIndirectIndexedY(Expression),
    SbcDirectPageIndirectLongIndexedY(Expression),
    SbcStackRelative(Expression),
    SbcStackRelativeIndirectIndexedY(Expression),

    SecImplied,
    SeiImplied,
    SedImplied,

    SepImmediate(Expression),

    StaAbsolute(Expression),
    StaAbsoluteLong(Expression),
    StaDirectPage(Expression),
    StaDirectPageIndirect(Expression),
    StaDirectPageIndirectLong(Expression),
    StaAbsoluteIndexedX(Expression),
    StaAbsoluteLongIndexedX(Expression),
    StaAbsoluteIndexedY(Expression),
    StaDirectPageIndexedX(Expression),
    StaDirectPageIndexedXIndirect(Expression),
    StaDirectPageIndirectIndexedY(Expression),
    StaDirectPageIndirectLongIndexedY(Expression),
    StaStackRelative(Expression),
    StaStackRelativeIndirectIndexedY(Expression),

    StpImplied,

    StxAbsolute(Expression),
    StxDirectPage(Expression),
    StxDirectPageIndexedY(Expression),

    StyAbsolute(Expression),
    StyDirectPage(Expression),
    StyDirectPageIndexedX(Expression),

    StzAbsolute(Expression),
    StzDirectPage(Expression),
    StzAbsoluteIndexedX(Expression),
    StzDirectPageIndexedX(Expression),

    TaxImplied,
    TayImplied,
    TcdImplied,
    TcsImplied,
    TdcImplied,
    TscImplied,
    TsxImplied,
    TxaImplied,
    TxsImplied,
    TxyImplied,
    TyaImplied,
    TyxImplied,

    TrbAbsolute(Expression),
    TrbDirectPage(Expression),
    TsbAbsolute(Expression),
    TsbDirectPage(Expression),

    WaiImplied,

    XbaImplied,
    XceImplied,
}
impl Instruction {
    pub fn size(&self) -> isize {
        match self {
            Instruction::AdcImmediate(_) => 3,
            Instruction::AdcAbsolute(_) => 3,
            Instruction::AdcAbsoluteLong(_) => 4,
            Instruction::AdcDirectPage(_) => 2,
            Instruction::AdcDirectPageIndirect(_) => 2,
            Instruction::AdcDirectPageIndirectLong(_) => 2,
            Instruction::AdcAbsoluteIndexedX(_) => 3,
            Instruction::AdcAbsoluteLongIndexedX(_) => 4,
            Instruction::AdcAbsoluteIndexedY(_) => 3,
            Instruction::AdcDirectPageIndexedX(_) => 2,
            Instruction::AdcDirectPageIndexedXIndirect(_) => 2,
            Instruction::AdcDirectPageIndirectIndexedY(_) => 2,
            Instruction::AdcDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::AdcStackRelative(_) => 2,
            Instruction::AdcStackRelativeIndirectIndexedY(_) => 2,

            Instruction::AndImmediate(_) => 3,
            Instruction::AndAbsolute(_) => 3,
            Instruction::AndAbsoluteLong(_) => 4,
            Instruction::AndDirectPage(_) => 2,
            Instruction::AndDirectPageIndirect(_) => 2,
            Instruction::AndDirectPageIndirectLong(_) => 2,
            Instruction::AndAbsoluteIndexedX(_) => 3,
            Instruction::AndAbsoluteLongIndexedX(_) => 4,
            Instruction::AndAbsoluteIndexedY(_) => 3,
            Instruction::AndDirectPageIndexedX(_) => 2,
            Instruction::AndDirectPageIndexedXIndirect(_) => 2,
            Instruction::AndDirectPageIndirectIndexedY(_) => 2,
            Instruction::AndDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::AndStackRelative(_) => 2,
            Instruction::AndStackRelativeIndirectIndexedY(_) => 2,

            Instruction::AslAccumulator => 1,
            Instruction::AslAbsolute(_) => 3,
            Instruction::AslDirectPage(_) => 2,
            Instruction::AslAbsoluteIndexedX(_) => 3,
            Instruction::AslDirectPageIndexedX(_) => 2,

            Instruction::BccRelative(_) => 2,
            Instruction::BcsRelative(_) => 2,
            Instruction::BeqRelative(_) => 2,
            Instruction::BneRelative(_) => 2,
            Instruction::BmiRelative(_) => 2,
            Instruction::BplRelative(_) => 2,
            Instruction::BvcRelative(_) => 2,
            Instruction::BvsRelative(_) => 2,
            Instruction::BraRelative(_) => 2,
            Instruction::BrlRelative(_) => 3,

            Instruction::BitImmediate(_) => 3,
            Instruction::BitAbsolute(_) => 3,
            Instruction::BitDirectPage(_) => 2,
            Instruction::BitAbsoluteIndexedX(_) => 3,
            Instruction::BitDirectPageIndexedX(_) => 2,

            Instruction::BrkImplied => 1,
            Instruction::CopImplied => 1,

            Instruction::ClcImplied => 1,
            Instruction::CliImplied => 1,
            Instruction::CldImplied => 1,
            Instruction::ClvImplied => 1,

            Instruction::CmpImmediate(_) => 3,
            Instruction::CmpAbsolute(_) => 3,
            Instruction::CmpAbsoluteLong(_) => 4,
            Instruction::CmpDirectPage(_) => 2,
            Instruction::CmpDirectPageIndirect(_) => 2,
            Instruction::CmpDirectPageIndirectLong(_) => 2,
            Instruction::CmpAbsoluteIndexedX(_) => 3,
            Instruction::CmpAbsoluteLongIndexedX(_) => 3,
            Instruction::CmpAbsoluteIndexedY(_) => 3,
            Instruction::CmpDirectPageIndexedX(_) => 2,
            Instruction::CmpDirectPageIndexedXIndirect(_) => 2,
            Instruction::CmpDirectPageIndirectIndexedY(_) => 2,
            Instruction::CmpDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::CmpStackRelative(_) => 2,
            Instruction::CmpStackRelativeIndirectIndexedY(_) => 2,

            Instruction::CpxImmediate(_) => 3,
            Instruction::CpxAbsolute(_) => 3,
            Instruction::CpxDirectPage(_) => 2,

            Instruction::CpyImmediate(_) => 3,
            Instruction::CpyAbsolute(_) => 3,
            Instruction::CpyDirectPage(_) => 2,

            Instruction::DecAccumulator => 1,
            Instruction::DecAbsolute(_) => 3,
            Instruction::DecDirectPage(_) => 2,
            Instruction::DecAbsoluteIndexedX(_) => 3,
            Instruction::DecDirectPageIndexedX(_) => 2,

            Instruction::DexImplied => 1,
            Instruction::DeyImplied => 1,

            Instruction::EorImmediate(_) => 3,
            Instruction::EorAbsolute(_) => 3,
            Instruction::EorAbsoluteLong(_) => 4,
            Instruction::EorDirectPage(_) => 2,
            Instruction::EorDirectPageIndirect(_) => 2,
            Instruction::EorDirectPageIndirectLong(_) => 2,
            Instruction::EorAbsoluteIndexedX(_) => 3,
            Instruction::EorAbsoluteLongIndexedX(_) => 4,
            Instruction::EorAbsoluteIndexedY(_) => 3,
            Instruction::EorDirectPageIndexedX(_) => 2,
            Instruction::EorDirectPageIndexedXIndirect(_) => 2,
            Instruction::EorDirectPageIndirectIndexedY(_) => 2,
            Instruction::EorDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::EorStackRelative(_) => 2,
            Instruction::EorStackRelativeIndirectIndexedY(_) => 2,

            Instruction::IncAccumulator => 1,
            Instruction::IncAbsolute(_) => 3,
            Instruction::IncDirectPage(_) => 2,
            Instruction::IncAbsoluteIndexedX(_) => 3,
            Instruction::IncDirectPageIndexedX(_) => 2,

            Instruction::InxImplied => 1,
            Instruction::InyImplied => 1,

            Instruction::JmpAbsolute(_) => 3,
            Instruction::JmpAbsoluteIndirect(_) => 3,
            Instruction::JmpAbsoluteIndexedXIndirect(_) => 3,
            Instruction::JmlAbsoluteLong(_) => 4,
            Instruction::JmlAbsoluteIndirectLong(_) => 3,

            Instruction::JsrAbsolute(_) => 3,
            Instruction::JsrAbsoluteIndexedXIndirect(_) => 3,
            Instruction::JslAbsoluteLong(_) => 4,

            Instruction::LdaImmediate(_, is_byte) => {
                if *is_byte {
                    2
                } else {
                    3
                }
            }
            Instruction::LdaAbsolute(_) => 3,
            Instruction::LdaAbsoluteLong(_) => 4,
            Instruction::LdaDirectPage(_) => 2,
            Instruction::LdaDirectPageIndirect(_) => 2,
            Instruction::LdaDirectPageIndirectLong(_) => 2,
            Instruction::LdaAbsoluteIndexedX(_) => 3,
            Instruction::LdaAbsoluteLongIndexedX(_) => 4,
            Instruction::LdaAbsoluteIndexedY(_) => 3,
            Instruction::LdaDirectPageIndexedX(_) => 2,
            Instruction::LdaDirectPageIndexedXIndirect(_) => 2,
            Instruction::LdaDirectPageIndirectIndexedY(_) => 2,
            Instruction::LdaDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::LdaStackRelative(_) => 2,
            Instruction::LdaStackRelativeIndirectIndexedY(_) => 2,

            Instruction::LdxImmediate(_) => 3,
            Instruction::LdxAbsolute(_) => 3,
            Instruction::LdxDirectPage(_) => 2,
            Instruction::LdxAbsoluteIndexedY(_) => 3,
            Instruction::LdxDirectPageIndexedY(_) => 2,

            Instruction::LdyImmediate(_) => 3,
            Instruction::LdyAbsolute(_) => 3,
            Instruction::LdyDirectPage(_) => 2,
            Instruction::LdyAbsoluteIndexedX(_) => 3,
            Instruction::LdyDirectPageIndexedX(_) => 2,

            Instruction::LsrAccumulator => 1,
            Instruction::LsrAbsolute(_) => 3,
            Instruction::LsrDirectPage(_) => 2,
            Instruction::LsrAbsoluteIndexedX(_) => 3,
            Instruction::LsrDirectPageIndexedX(_) => 2,

            Instruction::MvnBlockMove(_, _) => 3,
            Instruction::MvpBlockMove(_, _) => 3,

            Instruction::NopImplied => 1,

            Instruction::OraImmediate(_) => 3,
            Instruction::OraAbsolute(_) => 3,
            Instruction::OraAbsoluteLong(_) => 4,
            Instruction::OraDirectPage(_) => 2,
            Instruction::OraDirectPageIndirect(_) => 2,
            Instruction::OraDirectPageIndirectLong(_) => 2,
            Instruction::OraAbsoluteIndexedX(_) => 3,
            Instruction::OraAbsoluteLongIndexedX(_) => 4,
            Instruction::OraAbsoluteIndexedY(_) => 3,
            Instruction::OraDirectPageIndexedX(_) => 2,
            Instruction::OraDirectPageIndexedXIndirect(_) => 2,
            Instruction::OraDirectPageIndirectIndexedY(_) => 2,
            Instruction::OraDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::OraStackRelative(_) => 2,
            Instruction::OraStackRelativeIndirectIndexedY(_) => 2,

            Instruction::PeaAbsolute(_) => 3,
            Instruction::PeiDirectPageIndirect(_) => 2,
            Instruction::PerRelativeLong(_) => 3,

            Instruction::PhaImplied => 1,
            Instruction::PhbImplied => 1,
            Instruction::PhdImplied => 1,
            Instruction::PhkImplied => 1,
            Instruction::PhpImplied => 1,
            Instruction::PhxImplied => 1,
            Instruction::PhyImplied => 1,

            Instruction::PlaImplied => 1,
            Instruction::PlbImplied => 1,
            Instruction::PldImplied => 1,
            Instruction::PlpImplied => 1,
            Instruction::PlxImplied => 1,
            Instruction::PlyImplied => 1,

            Instruction::RepImmediate(_) => 2,

            Instruction::RolAccumulator => 1,
            Instruction::RolAbsolute(_) => 3,
            Instruction::RolDirectPage(_) => 2,
            Instruction::RolAbsoluteIndexedX(_) => 3,
            Instruction::RolDirectPageIndexedX(_) => 2,

            Instruction::RorAccumulator => 1,
            Instruction::RorAbsolute(_) => 3,
            Instruction::RorDirectPage(_) => 2,
            Instruction::RorAbsoluteIndexedX(_) => 3,
            Instruction::RorDirectPageIndexedX(_) => 2,

            Instruction::RtiImplied => 1,
            Instruction::RtsImplied => 1,
            Instruction::RtlImplied => 1,

            Instruction::SbcImmediate(_) => 3,
            Instruction::SbcAbsolute(_) => 3,
            Instruction::SbcAbsoluteLong(_) => 4,
            Instruction::SbcDirectPage(_) => 2,
            Instruction::SbcDirectPageIndirect(_) => 2,
            Instruction::SbcDirectPageIndirectLong(_) => 2,
            Instruction::SbcAbsoluteIndexedX(_) => 3,
            Instruction::SbcAbsoluteLongIndexedX(_) => 4,
            Instruction::SbcAbsoluteIndexedY(_) => 3,
            Instruction::SbcDirectPageIndexedX(_) => 2,
            Instruction::SbcDirectPageIndexedXIndirect(_) => 2,
            Instruction::SbcDirectPageIndirectIndexedY(_) => 2,
            Instruction::SbcDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::SbcStackRelative(_) => 2,
            Instruction::SbcStackRelativeIndirectIndexedY(_) => 2,

            Instruction::SecImplied => 1,
            Instruction::SeiImplied => 1,
            Instruction::SedImplied => 1,

            Instruction::SepImmediate(_) => 2,

            Instruction::StaAbsolute(_) => 3,
            Instruction::StaAbsoluteLong(_) => 4,
            Instruction::StaDirectPage(_) => 2,
            Instruction::StaDirectPageIndirect(_) => 2,
            Instruction::StaDirectPageIndirectLong(_) => 2,
            Instruction::StaAbsoluteIndexedX(_) => 3,
            Instruction::StaAbsoluteLongIndexedX(_) => 4,
            Instruction::StaAbsoluteIndexedY(_) => 3,
            Instruction::StaDirectPageIndexedX(_) => 2,
            Instruction::StaDirectPageIndexedXIndirect(_) => 2,
            Instruction::StaDirectPageIndirectIndexedY(_) => 2,
            Instruction::StaDirectPageIndirectLongIndexedY(_) => 2,
            Instruction::StaStackRelative(_) => 2,
            Instruction::StaStackRelativeIndirectIndexedY(_) => 2,

            Instruction::StpImplied => 1,

            Instruction::StxAbsolute(_) => 3,
            Instruction::StxDirectPage(_) => 2,
            Instruction::StxDirectPageIndexedY(_) => 2,

            Instruction::StyAbsolute(_) => 3,
            Instruction::StyDirectPage(_) => 2,
            Instruction::StyDirectPageIndexedX(_) => 2,

            Instruction::StzAbsolute(_) => 3,
            Instruction::StzDirectPage(_) => 2,
            Instruction::StzAbsoluteIndexedX(_) => 3,
            Instruction::StzDirectPageIndexedX(_) => 2,

            Instruction::TaxImplied => 1,
            Instruction::TayImplied => 1,
            Instruction::TcdImplied => 1,
            Instruction::TcsImplied => 1,
            Instruction::TdcImplied => 1,
            Instruction::TscImplied => 1,
            Instruction::TsxImplied => 1,
            Instruction::TxaImplied => 1,
            Instruction::TxsImplied => 1,
            Instruction::TxyImplied => 1,
            Instruction::TyaImplied => 1,
            Instruction::TyxImplied => 1,

            Instruction::TrbAbsolute(_) => 3,
            Instruction::TrbDirectPage(_) => 2,
            Instruction::TsbAbsolute(_) => 3,
            Instruction::TsbDirectPage(_) => 2,

            Instruction::WaiImplied => 1,

            Instruction::XbaImplied => 1,
            Instruction::XceImplied => 1,
        }
    }
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::AdcImmediate(expr) => write!(f, "ADC #{}", expr.to_hex(2, false)),
            Instruction::AdcAbsolute(expr) => write!(f, "ADC {}", expr.to_hex(2, false)),
            Instruction::AdcAbsoluteLong(expr) => {
                write!(f, "ADC {}", expr.to_hex(3, false))
            }
            Instruction::AdcDirectPage(expr) => write!(f, "ADC {}", expr.to_hex(1, false)),
            Instruction::AdcDirectPageIndirect(expr) => {
                write!(f, "ADC ({})", expr.to_hex(1, false))
            }
            Instruction::AdcDirectPageIndirectLong(expr) => {
                write!(f, "ADC [{}]", expr.to_hex(1, false))
            }
            Instruction::AdcAbsoluteIndexedX(expr) => {
                write!(f, "ADC {},X", expr.to_hex(2, false))
            }
            Instruction::AdcAbsoluteLongIndexedX(expr) => {
                write!(f, "ADC {},X", expr.to_hex(3, false))
            }
            Instruction::AdcAbsoluteIndexedY(expr) => {
                write!(f, "ADC {},Y", expr.to_hex(2, false))
            }
            Instruction::AdcDirectPageIndexedX(expr) => {
                write!(f, "ADC {},X", expr.to_hex(1, false))
            }
            Instruction::AdcDirectPageIndexedXIndirect(expr) => {
                write!(f, "ADC ({},X)", expr.to_hex(1, false))
            }
            Instruction::AdcDirectPageIndirectIndexedY(expr) => {
                write!(f, "ADC ({}),Y", expr.to_hex(1, false))
            }
            Instruction::AdcDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "ADC [{}],Y", expr.to_hex(1, false))
            }
            Instruction::AdcStackRelative(expr) => {
                write!(f, "ADC {},S", expr.to_hex(1, false))
            }
            Instruction::AdcStackRelativeIndirectIndexedY(expr) => {
                write!(f, "ADC ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::AndImmediate(expr) => write!(f, "AND #{}", expr.to_hex(2, false)),
            Instruction::AndAbsolute(expr) => write!(f, "AND {}", expr.to_hex(2, false)),
            Instruction::AndAbsoluteLong(expr) => {
                write!(f, "AND {}", expr.to_hex(3, false))
            }
            Instruction::AndDirectPage(expr) => write!(f, "AND {}", expr.to_hex(1, false)),
            Instruction::AndDirectPageIndirect(expr) => {
                write!(f, "AND ({})", expr.to_hex(1, false))
            }
            Instruction::AndDirectPageIndirectLong(expr) => {
                write!(f, "AND [{}]", expr.to_hex(1, false))
            }
            Instruction::AndAbsoluteIndexedX(expr) => {
                write!(f, "AND {},X", expr.to_hex(2, false))
            }
            Instruction::AndAbsoluteLongIndexedX(expr) => {
                write!(f, "AND {},X", expr.to_hex(3, false))
            }
            Instruction::AndAbsoluteIndexedY(expr) => {
                write!(f, "AND {},Y", expr.to_hex(2, false))
            }
            Instruction::AndDirectPageIndexedX(expr) => {
                write!(f, "AND {},X", expr.to_hex(1, false))
            }
            Instruction::AndDirectPageIndexedXIndirect(expr) => {
                write!(f, "AND ({},X)", expr.to_hex(1, false))
            }
            Instruction::AndDirectPageIndirectIndexedY(expr) => {
                write!(f, "AND ({}),Y", expr.to_hex(1, false))
            }
            Instruction::AndDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "AND [{}],Y", expr.to_hex(1, false))
            }
            Instruction::AndStackRelative(expr) => {
                write!(f, "AND {},S", expr.to_hex(1, false))
            }
            Instruction::AndStackRelativeIndirectIndexedY(expr) => {
                write!(f, "AND ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::AslAccumulator => write!(f, "ASL"),
            Instruction::AslAbsolute(expr) => write!(f, "ASL {}", expr.to_hex(2, false)),
            Instruction::AslDirectPage(expr) => write!(f, "ASL {}", expr.to_hex(1, false)),
            Instruction::AslAbsoluteIndexedX(expr) => {
                write!(f, "ASL {},X", expr.to_hex(2, false))
            }
            Instruction::AslDirectPageIndexedX(expr) => {
                write!(f, "ASL {},X", expr.to_hex(1, false))
            }

            Instruction::BccRelative(label) => {
                write!(f, "BCC {}", label)
            }
            Instruction::BcsRelative(label) => {
                write!(f, "BCS {}", label)
            }
            Instruction::BeqRelative(label) => {
                write!(f, "BEQ {}", label)
            }
            Instruction::BneRelative(label) => {
                write!(f, "BNE {}", label)
            }
            Instruction::BmiRelative(label) => {
                write!(f, "BMI {}", label)
            }
            Instruction::BplRelative(label) => {
                write!(f, "BPL {}", label)
            }
            Instruction::BvcRelative(label) => {
                write!(f, "BVC {}", label)
            }
            Instruction::BvsRelative(label) => {
                write!(f, "BVS {}", label)
            }
            Instruction::BraRelative(label) => {
                write!(f, "BRA {}", label)
            }
            Instruction::BrlRelative(label) => {
                write!(f, "BRL {}", label)
            }

            Instruction::BitImmediate(expr) => write!(f, "BIT #{}", expr.to_hex(2, false)),
            Instruction::BitAbsolute(expr) => write!(f, "BIT {}", expr.to_hex(2, false)),
            Instruction::BitDirectPage(expr) => write!(f, "BIT {}", expr.to_hex(1, false)),
            Instruction::BitAbsoluteIndexedX(expr) => {
                write!(f, "BIT {},X", expr.to_hex(2, false))
            }
            Instruction::BitDirectPageIndexedX(expr) => {
                write!(f, "BIT {},X", expr.to_hex(1, false))
            }

            Instruction::BrkImplied => write!(f, "BRK"),
            Instruction::CopImplied => write!(f, "COP"),

            Instruction::ClcImplied => write!(f, "CLC"),
            Instruction::CliImplied => write!(f, "CLI"),
            Instruction::CldImplied => write!(f, "CLD"),
            Instruction::ClvImplied => write!(f, "CLV"),

            Instruction::CmpImmediate(expr) => write!(f, "CMP #{}", expr.to_hex(2, false)),
            Instruction::CmpAbsolute(expr) => write!(f, "CMP {}", expr.to_hex(2, false)),
            Instruction::CmpAbsoluteLong(expr) => {
                write!(f, "CMP {}", expr.to_hex(3, false))
            }
            Instruction::CmpDirectPage(expr) => write!(f, "CMP {}", expr.to_hex(1, false)),
            Instruction::CmpDirectPageIndirect(expr) => {
                write!(f, "CMP ({})", expr.to_hex(1, false))
            }
            Instruction::CmpDirectPageIndirectLong(expr) => {
                write!(f, "CMP [{}]", expr.to_hex(1, false))
            }
            Instruction::CmpAbsoluteIndexedX(expr) => {
                write!(f, "CMP {},X", expr.to_hex(2, false))
            }
            Instruction::CmpAbsoluteLongIndexedX(expr) => {
                write!(f, "CMP {},X", expr.to_hex(3, false))
            }
            Instruction::CmpAbsoluteIndexedY(expr) => {
                write!(f, "CMP {},Y", expr.to_hex(2, false))
            }
            Instruction::CmpDirectPageIndexedX(expr) => {
                write!(f, "CMP {},X", expr.to_hex(1, false))
            }
            Instruction::CmpDirectPageIndexedXIndirect(expr) => {
                write!(f, "CMP ({},X)", expr.to_hex(1, false))
            }
            Instruction::CmpDirectPageIndirectIndexedY(expr) => {
                write!(f, "CMP ({}),Y", expr.to_hex(1, false))
            }
            Instruction::CmpDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "CMP [{}],Y", expr.to_hex(1, false))
            }
            Instruction::CmpStackRelative(expr) => {
                write!(f, "CMP {},S", expr.to_hex(1, false))
            }
            Instruction::CmpStackRelativeIndirectIndexedY(expr) => {
                write!(f, "CMP ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::CpxImmediate(expr) => write!(f, "CPX #{}", expr.to_hex(2, false)),
            Instruction::CpxAbsolute(expr) => write!(f, "CPX {}", expr.to_hex(2, false)),
            Instruction::CpxDirectPage(expr) => write!(f, "CPX {}", expr.to_hex(1, false)),

            Instruction::CpyImmediate(expr) => write!(f, "CPY #{}", expr.to_hex(2, false)),
            Instruction::CpyAbsolute(expr) => write!(f, "CPY {}", expr.to_hex(2, false)),
            Instruction::CpyDirectPage(expr) => write!(f, "CPY {}", expr.to_hex(1, false)),

            Instruction::DecAccumulator => write!(f, "INC"),
            Instruction::DecAbsolute(expr) => write!(f, "INC {}", expr.to_hex(2, false)),
            Instruction::DecDirectPage(expr) => write!(f, "INC {}", expr.to_hex(1, false)),
            Instruction::DecAbsoluteIndexedX(expr) => {
                write!(f, "INC {},X", expr.to_hex(2, false))
            }
            Instruction::DecDirectPageIndexedX(expr) => {
                write!(f, "INC {},X", expr.to_hex(1, false))
            }

            Instruction::DexImplied => write!(f, "DEX"),
            Instruction::DeyImplied => write!(f, "DEY"),

            Instruction::EorImmediate(expr) => write!(f, "EOR #{}", expr.to_hex(2, false)),
            Instruction::EorAbsolute(expr) => write!(f, "EOR {}", expr.to_hex(2, false)),
            Instruction::EorAbsoluteLong(expr) => {
                write!(f, "EOR {}", expr.to_hex(3, false))
            }
            Instruction::EorDirectPage(expr) => write!(f, "EOR {}", expr.to_hex(1, false)),
            Instruction::EorDirectPageIndirect(expr) => {
                write!(f, "EOR ({})", expr.to_hex(1, false))
            }
            Instruction::EorDirectPageIndirectLong(expr) => {
                write!(f, "EOR [{}]", expr.to_hex(1, false))
            }
            Instruction::EorAbsoluteIndexedX(expr) => {
                write!(f, "EOR {},X", expr.to_hex(2, false))
            }
            Instruction::EorAbsoluteLongIndexedX(expr) => {
                write!(f, "EOR {},X", expr.to_hex(3, false))
            }
            Instruction::EorAbsoluteIndexedY(expr) => {
                write!(f, "EOR {},Y", expr.to_hex(2, false))
            }
            Instruction::EorDirectPageIndexedX(expr) => {
                write!(f, "EOR {},X", expr.to_hex(1, false))
            }
            Instruction::EorDirectPageIndexedXIndirect(expr) => {
                write!(f, "EOR ({},X)", expr.to_hex(1, false))
            }
            Instruction::EorDirectPageIndirectIndexedY(expr) => {
                write!(f, "EOR ({}),Y", expr.to_hex(1, false))
            }
            Instruction::EorDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "EOR [{}],Y", expr.to_hex(1, false))
            }
            Instruction::EorStackRelative(expr) => {
                write!(f, "EOR {},S", expr.to_hex(1, false))
            }
            Instruction::EorStackRelativeIndirectIndexedY(expr) => {
                write!(f, "EOR ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::IncAccumulator => write!(f, "INC"),
            Instruction::IncAbsolute(expr) => write!(f, "INC {}", expr.to_hex(2, false)),
            Instruction::IncDirectPage(expr) => write!(f, "INC {}", expr.to_hex(1, false)),
            Instruction::IncAbsoluteIndexedX(expr) => {
                write!(f, "INC {},X", expr.to_hex(2, false))
            }
            Instruction::IncDirectPageIndexedX(expr) => {
                write!(f, "INC {},X", expr.to_hex(1, false))
            }

            Instruction::InxImplied => write!(f, "INX"),
            Instruction::InyImplied => write!(f, "INY"),

            Instruction::JmpAbsolute(expr) => write!(f, "JMP {}", expr.to_hex(2, false)),
            Instruction::JmpAbsoluteIndirect(expr) => {
                write!(f, "JMP ({})", expr.to_hex(2, false))
            }
            Instruction::JmpAbsoluteIndexedXIndirect(expr) => {
                write!(f, "JMP ({},X)", expr.to_hex(2, false))
            }
            Instruction::JmlAbsoluteLong(expr) => {
                write!(f, "JML {}", expr.to_hex(3, false))
            }
            Instruction::JmlAbsoluteIndirectLong(expr) => {
                write!(f, "JML [{}]", expr.to_hex(2, false))
            }

            Instruction::JsrAbsolute(expr) => write!(f, "JSR {}", expr.to_hex(2, false)),
            Instruction::JsrAbsoluteIndexedXIndirect(expr) => {
                write!(f, "JSR ({},X)", expr.to_hex(2, false))
            }
            Instruction::JslAbsoluteLong(expr) => {
                write!(f, "JSL {}", expr.to_hex(3, false))
            }

            Instruction::LdaImmediate(expr, byte_mode) => write!(
                f,
                "LDA #{}",
                expr.to_hex(if *byte_mode { 1 } else { 2 }, false)
            ),
            Instruction::LdaAbsolute(expr) => write!(f, "LDA {}", expr.to_hex(2, false)),
            Instruction::LdaAbsoluteLong(expr) => {
                write!(f, "LDA {}", expr.to_hex(3, false))
            }
            Instruction::LdaDirectPage(expr) => write!(f, "LDA {}", expr.to_hex(1, false)),
            Instruction::LdaDirectPageIndirect(expr) => {
                write!(f, "LDA ({})", expr.to_hex(1, false))
            }
            Instruction::LdaDirectPageIndirectLong(expr) => {
                write!(f, "LDA [{}]", expr.to_hex(1, false))
            }
            Instruction::LdaAbsoluteIndexedX(expr) => {
                write!(f, "LDA {},X", expr.to_hex(2, false))
            }
            Instruction::LdaAbsoluteLongIndexedX(expr) => {
                write!(f, "LDA {},X", expr.to_hex(3, false))
            }
            Instruction::LdaAbsoluteIndexedY(expr) => {
                write!(f, "LDA {},Y", expr.to_hex(2, false))
            }
            Instruction::LdaDirectPageIndexedX(expr) => {
                write!(f, "LDA {},X", expr.to_hex(1, false))
            }
            Instruction::LdaDirectPageIndexedXIndirect(expr) => {
                write!(f, "LDA ({},X)", expr.to_hex(1, false))
            }
            Instruction::LdaDirectPageIndirectIndexedY(expr) => {
                write!(f, "LDA ({}),Y", expr.to_hex(1, false))
            }
            Instruction::LdaDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "LDA [{}],Y", expr.to_hex(1, false))
            }
            Instruction::LdaStackRelative(expr) => {
                write!(f, "LDA {},S", expr.to_hex(1, false))
            }
            Instruction::LdaStackRelativeIndirectIndexedY(expr) => {
                write!(f, "LDA ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::LdxImmediate(expr) => write!(f, "LDX #{}", expr.to_hex(2, false)),
            Instruction::LdxAbsolute(expr) => write!(f, "LDX {}", expr.to_hex(2, false)),
            Instruction::LdxDirectPage(expr) => write!(f, "LDX {}", expr.to_hex(1, false)),
            Instruction::LdxAbsoluteIndexedY(expr) => {
                write!(f, "LDX {},Y", expr.to_hex(2, false))
            }
            Instruction::LdxDirectPageIndexedY(expr) => {
                write!(f, "LDX {},Y", expr.to_hex(1, false))
            }

            Instruction::LdyImmediate(expr) => write!(f, "LDY #{}", expr.to_hex(2, false)),
            Instruction::LdyAbsolute(expr) => write!(f, "LDY {}", expr.to_hex(2, false)),
            Instruction::LdyDirectPage(expr) => write!(f, "LDY {}", expr.to_hex(1, false)),
            Instruction::LdyAbsoluteIndexedX(expr) => {
                write!(f, "LDY {},X", expr.to_hex(2, false))
            }
            Instruction::LdyDirectPageIndexedX(expr) => {
                write!(f, "LDY {},X", expr.to_hex(1, false))
            }

            Instruction::LsrAccumulator => write!(f, "LSR"),
            Instruction::LsrAbsolute(expr) => write!(f, "LSR {}", expr.to_hex(2, false)),
            Instruction::LsrDirectPage(expr) => write!(f, "LSR {}", expr.to_hex(1, false)),
            Instruction::LsrAbsoluteIndexedX(expr) => {
                write!(f, "LSR {},X", expr.to_hex(2, false))
            }
            Instruction::LsrDirectPageIndexedX(expr) => {
                write!(f, "LSR {},X", expr.to_hex(1, false))
            }

            Instruction::MvnBlockMove(expr1, expr2) => write!(
                f,
                "MVN {}, {}",
                expr1.to_hex(1, false),
                expr2.to_hex(1, false)
            ),
            Instruction::MvpBlockMove(expr1, expr2) => write!(
                f,
                "MVP {}, {}",
                expr1.to_hex(1, false),
                expr2.to_hex(1, false)
            ),

            Instruction::NopImplied => write!(f, "NOP"),

            Instruction::OraImmediate(expr) => write!(f, "ORA #{}", expr.to_hex(2, false)),
            Instruction::OraAbsolute(expr) => write!(f, "ORA {}", expr.to_hex(2, false)),
            Instruction::OraAbsoluteLong(expr) => {
                write!(f, "ORA {}", expr.to_hex(3, false))
            }
            Instruction::OraDirectPage(expr) => write!(f, "ORA {}", expr.to_hex(1, false)),
            Instruction::OraDirectPageIndirect(expr) => {
                write!(f, "ORA ({})", expr.to_hex(1, false))
            }
            Instruction::OraDirectPageIndirectLong(expr) => {
                write!(f, "ORA [{}]", expr.to_hex(1, false))
            }
            Instruction::OraAbsoluteIndexedX(expr) => {
                write!(f, "ORA {},X", expr.to_hex(2, false))
            }
            Instruction::OraAbsoluteLongIndexedX(expr) => {
                write!(f, "ORA {},X", expr.to_hex(3, false))
            }
            Instruction::OraAbsoluteIndexedY(expr) => {
                write!(f, "ORA {},Y", expr.to_hex(2, false))
            }
            Instruction::OraDirectPageIndexedX(expr) => {
                write!(f, "ORA {},X", expr.to_hex(1, false))
            }
            Instruction::OraDirectPageIndexedXIndirect(expr) => {
                write!(f, "ORA ({},X)", expr.to_hex(1, false))
            }
            Instruction::OraDirectPageIndirectIndexedY(expr) => {
                write!(f, "ORA ({}),Y", expr.to_hex(1, false))
            }
            Instruction::OraDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "ORA [{}],Y", expr.to_hex(1, false))
            }
            Instruction::OraStackRelative(expr) => {
                write!(f, "ORA {},S", expr.to_hex(1, false))
            }
            Instruction::OraStackRelativeIndirectIndexedY(expr) => {
                write!(f, "ORA ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::PeaAbsolute(expr) => write!(f, "PEA {}", expr.to_hex(2, false)),
            Instruction::PeiDirectPageIndirect(expr) => {
                write!(f, "PEI ({})", expr.to_hex(1, false))
            }
            Instruction::PerRelativeLong(label) => {
                write!(f, "PER {}", label)
            }

            Instruction::PhaImplied => write!(f, "PHA"),
            Instruction::PhbImplied => write!(f, "PHB"),
            Instruction::PhdImplied => write!(f, "PHD"),
            Instruction::PhkImplied => write!(f, "PHK"),
            Instruction::PhpImplied => write!(f, "PHP"),
            Instruction::PhxImplied => write!(f, "PHX"),
            Instruction::PhyImplied => write!(f, "PHY"),

            Instruction::PlaImplied => write!(f, "PLA"),
            Instruction::PlbImplied => write!(f, "PLB"),
            Instruction::PldImplied => write!(f, "PLD"),
            Instruction::PlpImplied => write!(f, "PLP"),
            Instruction::PlxImplied => write!(f, "PLX"),
            Instruction::PlyImplied => write!(f, "PLY"),

            Instruction::RepImmediate(expr) => write!(f, "REP #{}", expr.to_hex(1, false)),

            Instruction::RolAccumulator => write!(f, "ROL"),
            Instruction::RolAbsolute(expr) => write!(f, "ROL {}", expr.to_hex(2, false)),
            Instruction::RolDirectPage(expr) => write!(f, "ROL {}", expr.to_hex(1, false)),
            Instruction::RolAbsoluteIndexedX(expr) => {
                write!(f, "ROL {},X", expr.to_hex(2, false))
            }
            Instruction::RolDirectPageIndexedX(expr) => {
                write!(f, "ROL {},X", expr.to_hex(1, false))
            }

            Instruction::RorAccumulator => write!(f, "ROR"),
            Instruction::RorAbsolute(expr) => write!(f, "ROR {}", expr.to_hex(2, false)),
            Instruction::RorDirectPage(expr) => write!(f, "ROR {}", expr.to_hex(1, false)),
            Instruction::RorAbsoluteIndexedX(expr) => {
                write!(f, "ROR {},X", expr.to_hex(2, false))
            }
            Instruction::RorDirectPageIndexedX(expr) => {
                write!(f, "ROR {},X", expr.to_hex(1, false))
            }

            Instruction::RtiImplied => write!(f, "RTI"),
            Instruction::RtsImplied => write!(f, "RTS"),
            Instruction::RtlImplied => write!(f, "RTL"),

            Instruction::SbcImmediate(expr) => write!(f, "SBC #{}", expr.to_hex(2, false)),
            Instruction::SbcAbsolute(expr) => write!(f, "SBC {}", expr.to_hex(2, false)),
            Instruction::SbcAbsoluteLong(expr) => {
                write!(f, "SBC {}", expr.to_hex(3, false))
            }
            Instruction::SbcDirectPage(expr) => write!(f, "SBC {}", expr.to_hex(1, false)),
            Instruction::SbcDirectPageIndirect(expr) => {
                write!(f, "SBC ({})", expr.to_hex(1, false))
            }
            Instruction::SbcDirectPageIndirectLong(expr) => {
                write!(f, "SBC [{}]", expr.to_hex(1, false))
            }
            Instruction::SbcAbsoluteIndexedX(expr) => {
                write!(f, "SBC {},X", expr.to_hex(2, false))
            }
            Instruction::SbcAbsoluteLongIndexedX(expr) => {
                write!(f, "SBC {},X", expr.to_hex(3, false))
            }
            Instruction::SbcAbsoluteIndexedY(expr) => {
                write!(f, "SBC {},Y", expr.to_hex(2, false))
            }
            Instruction::SbcDirectPageIndexedX(expr) => {
                write!(f, "SBC {},X", expr.to_hex(1, false))
            }
            Instruction::SbcDirectPageIndexedXIndirect(expr) => {
                write!(f, "SBC ({},X)", expr.to_hex(1, false))
            }
            Instruction::SbcDirectPageIndirectIndexedY(expr) => {
                write!(f, "SBC ({}),Y", expr.to_hex(1, false))
            }
            Instruction::SbcDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "SBC [{}],Y", expr.to_hex(1, false))
            }
            Instruction::SbcStackRelative(expr) => {
                write!(f, "SBC {},S", expr.to_hex(1, false))
            }
            Instruction::SbcStackRelativeIndirectIndexedY(expr) => {
                write!(f, "SBC ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::SecImplied => write!(f, "SEC"),
            Instruction::SeiImplied => write!(f, "SEI"),
            Instruction::SedImplied => write!(f, "SED"),

            Instruction::SepImmediate(expr) => write!(f, "SEP #{}", expr.to_hex(1, false)),

            Instruction::StaAbsolute(expr) => write!(f, "STA {}", expr.to_hex(2, false)),
            Instruction::StaAbsoluteLong(expr) => {
                write!(f, "STA {}", expr.to_hex(3, false))
            }
            Instruction::StaDirectPage(expr) => write!(f, "STA {}", expr.to_hex(1, false)),
            Instruction::StaDirectPageIndirect(expr) => {
                write!(f, "STA ({})", expr.to_hex(1, false))
            }
            Instruction::StaDirectPageIndirectLong(expr) => {
                write!(f, "STA [{}]", expr.to_hex(1, false))
            }
            Instruction::StaAbsoluteIndexedX(expr) => {
                write!(f, "STA {},X", expr.to_hex(2, false))
            }
            Instruction::StaAbsoluteLongIndexedX(expr) => {
                write!(f, "STA {},X", expr.to_hex(3, false))
            }
            Instruction::StaAbsoluteIndexedY(expr) => {
                write!(f, "STA {},Y", expr.to_hex(2, false))
            }
            Instruction::StaDirectPageIndexedX(expr) => {
                write!(f, "STA {},X", expr.to_hex(1, false))
            }
            Instruction::StaDirectPageIndexedXIndirect(expr) => {
                write!(f, "STA ({},X)", expr.to_hex(1, false))
            }
            Instruction::StaDirectPageIndirectIndexedY(expr) => {
                write!(f, "STA ({}),Y", expr.to_hex(1, false))
            }
            Instruction::StaDirectPageIndirectLongIndexedY(expr) => {
                write!(f, "STA [{}],Y", expr.to_hex(1, false))
            }
            Instruction::StaStackRelative(expr) => {
                write!(f, "STA {},S", expr.to_hex(1, false))
            }
            Instruction::StaStackRelativeIndirectIndexedY(expr) => {
                write!(f, "STA ({},S),Y", expr.to_hex(1, false))
            }

            Instruction::StpImplied => write!(f, "STP"),

            Instruction::StxAbsolute(expr) => write!(f, "STX {}", expr.to_hex(2, false)),
            Instruction::StxDirectPage(expr) => write!(f, "STX {}", expr.to_hex(1, false)),
            Instruction::StxDirectPageIndexedY(expr) => {
                write!(f, "STX {},Y", expr.to_hex(1, false))
            }

            Instruction::StyAbsolute(expr) => write!(f, "STY {}", expr.to_hex(2, false)),
            Instruction::StyDirectPage(expr) => write!(f, "STY {}", expr.to_hex(1, false)),
            Instruction::StyDirectPageIndexedX(expr) => {
                write!(f, "STY {},X", expr.to_hex(1, false))
            }

            Instruction::StzAbsolute(expr) => write!(f, "STZ {}", expr.to_hex(2, false)),
            Instruction::StzDirectPage(expr) => write!(f, "STZ {}", expr.to_hex(1, false)),
            Instruction::StzAbsoluteIndexedX(expr) => {
                write!(f, "STZ {},X", expr.to_hex(2, false))
            }
            Instruction::StzDirectPageIndexedX(expr) => {
                write!(f, "STZ {},X", expr.to_hex(1, false))
            }

            Instruction::TaxImplied => write!(f, "TAX"),
            Instruction::TayImplied => write!(f, "TAY"),
            Instruction::TcdImplied => write!(f, "TCD"),
            Instruction::TcsImplied => write!(f, "TCS"),
            Instruction::TdcImplied => write!(f, "TDC"),
            Instruction::TscImplied => write!(f, "TSC"),
            Instruction::TsxImplied => write!(f, "TSX"),
            Instruction::TxaImplied => write!(f, "TXA"),
            Instruction::TxsImplied => write!(f, "TXS"),
            Instruction::TxyImplied => write!(f, "TXY"),
            Instruction::TyaImplied => write!(f, "TYA"),
            Instruction::TyxImplied => write!(f, "TYX"),

            Instruction::TrbAbsolute(expr) => write!(f, "TRB {}", expr.to_hex(2, false)),
            Instruction::TrbDirectPage(expr) => write!(f, "TRB {}", expr.to_hex(1, false)),
            Instruction::TsbAbsolute(expr) => write!(f, "TSB {}", expr.to_hex(2, false)),
            Instruction::TsbDirectPage(expr) => write!(f, "TSB {}", expr.to_hex(1, false)),

            Instruction::WaiImplied => write!(f, "WAI"),

            Instruction::XbaImplied => write!(f, "XBA"),
            Instruction::XceImplied => write!(f, "XCE"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Directive {
    Origin(Expression),
    StoreByte(DisplayableVec<Expression>),
    StoreWord(DisplayableVec<Expression>),
    AsciiString(String, bool),
    UnicodeString(String, bool),
}
impl Directive {
    pub fn size(&self) -> isize {
        match self {
            Directive::Origin(_) => 0,
            Directive::StoreByte(bytes) => bytes.0.len() as isize,
            Directive::StoreWord(words) => words.0.len() as isize * 2,
            Directive::AsciiString(s, null_terminated) => {
                s.len() as isize + if *null_terminated { 1 } else { 0 }
            }
            Directive::UnicodeString(s, null_terminated) => {
                s.len() as isize * 2 + if *null_terminated { 2 } else { 0 }
            }
        }
    }
}
impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Origin(expr) => {
                write!(f, ".org {}", expr.to_hex(3, false))
            }
            Directive::StoreByte(exprs) => {
                write!(f, ".byte {}", exprs.to_hex(1, false))
            }
            Directive::StoreWord(exprs) => {
                write!(f, ".word {}", exprs.to_hex(2, false))
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
impl Statement {
    fn size(&self) -> isize {
        match self {
            Statement::Instruction(inst) => inst.size(),
            Statement::Directive(dir) => dir.size(),
            Statement::Label(_) => 0,
            Statement::Empty => 0,
        }
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Instruction(inst) => {
                write!(f, "\t{}", inst)
            }
            Statement::Directive(dir) => {
                write!(f, "{}", dir)
            }
            Statement::Label(l) => {
                write!(f, "{}:", l)
            }
            Statement::Empty => {
                write!(f, "")
            }
        }
    }
}

#[derive(Debug)]
pub struct Program(pub Vec<Statement>);
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stm in self.0.iter() {
            writeln!(f, "{}", stm)?;
        }
        Ok(())
    }
}

fn resolve_origin_expression(expr: &Expression) -> Wrapping<isize> {
    match expr {
        Expression::Literal(value) => *value,
        Expression::Label(_) => Wrapping(0),
        Expression::Identifier(_) => unreachable!(),
        Expression::UnaryOperator(op_type, base_expr) => {
            let base_value = resolve_origin_expression(base_expr);
            match op_type {
                UnaryOperatorType::Positive => base_value,
                UnaryOperatorType::Negative => -base_value,
                UnaryOperatorType::BitwiseNot => !base_value,
            }
        }
        Expression::BinaryOperator(op_type, left, right) => {
            let left_value = resolve_origin_expression(left);
            let right_value = resolve_origin_expression(right);
            match op_type {
                BinaryOperatorType::Multiply => left_value * right_value,
                BinaryOperatorType::Divide => left_value / right_value,
                BinaryOperatorType::Remainder => left_value % right_value,
                BinaryOperatorType::Add => left_value + right_value,
                BinaryOperatorType::Subtract => left_value - right_value,
                BinaryOperatorType::ShiftLeft => left_value << right_value.0 as usize,
                BinaryOperatorType::ShiftRight => left_value >> right_value.0 as usize,
                BinaryOperatorType::BitwiseAnd => left_value & right_value,
                BinaryOperatorType::BitwiseXor => left_value ^ right_value,
                BinaryOperatorType::BitwiseOr => left_value | right_value,
            }
        }
    }
}

fn assign_label_values(prog: &Program) -> HashMap<&str, isize> {
    let mut result: HashMap<&str, isize> = HashMap::new();
    let mut ptr: isize = 0;
    for stm in prog.0.iter() {
        if let Statement::Directive(Directive::Origin(expr)) = stm {
            ptr = resolve_origin_expression(expr).0;
        }
        if let Statement::Label(name) = stm {
            result.insert(name, ptr);
        }
        ptr += stm.size();
    }
    result
}

fn resolve_expression(expr: &Expression, label_values: &HashMap<&str, isize>) -> Wrapping<isize> {
    match expr {
        Expression::Literal(value) => *value,
        Expression::Label(name) => {
            let n: &str = name;
            Wrapping(*label_values.get(&n).unwrap() as isize)
        }
        Expression::Identifier(_) => unreachable!(),
        Expression::UnaryOperator(op_type, base_expr) => {
            let base_value = resolve_expression(base_expr, label_values);
            match op_type {
                UnaryOperatorType::Positive => base_value,
                UnaryOperatorType::Negative => -base_value,
                UnaryOperatorType::BitwiseNot => !base_value,
            }
        }
        Expression::BinaryOperator(op_type, left, right) => {
            let left_value = resolve_expression(left, label_values);
            let right_value = resolve_expression(right, label_values);
            match op_type {
                BinaryOperatorType::Multiply => left_value * right_value,
                BinaryOperatorType::Divide => left_value / right_value,
                BinaryOperatorType::Remainder => left_value % right_value,
                BinaryOperatorType::Add => left_value + right_value,
                BinaryOperatorType::Subtract => left_value - right_value,
                BinaryOperatorType::ShiftLeft => left_value << right_value.0 as usize,
                BinaryOperatorType::ShiftRight => left_value >> right_value.0 as usize,
                BinaryOperatorType::BitwiseAnd => left_value & right_value,
                BinaryOperatorType::BitwiseXor => left_value ^ right_value,
                BinaryOperatorType::BitwiseOr => left_value | right_value,
            }
        }
    }
}

fn resolve_u8(expr: &Expression, label_values: &HashMap<&str, isize>) -> u8 {
    resolve_expression(expr, label_values).0 as u8
}

fn resolve_u16(expr: &Expression, label_values: &HashMap<&str, isize>) -> u16 {
    resolve_expression(expr, label_values).0 as u16
}

fn resolve_u24(expr: &Expression, label_values: &HashMap<&str, isize>) -> u32 {
    (resolve_expression(expr, label_values).0 as u32) & 0x00FFFFFF
}

fn assemble_arg_1(op_code: u8, arg: &Expression, label_values: &HashMap<&str, isize>) -> Vec<u8> {
    let a = resolve_u8(arg, label_values);
    vec![op_code, a]
}

fn assemble_arg_2(op_code: u8, arg: &Expression, label_values: &HashMap<&str, isize>) -> Vec<u8> {
    let a = resolve_u16(arg, label_values);
    let l = (a & 0x00FF) as u8;
    let h = ((a & 0xFF00) >> 8) as u8;
    vec![op_code, l, h]
}

fn assemble_arg_3(op_code: u8, arg: &Expression, label_values: &HashMap<&str, isize>) -> Vec<u8> {
    let a = resolve_u24(arg, label_values);
    let l = (a & 0x0000FF) as u8;
    let h = ((a & 0x00FF00) >> 8) as u8;
    let b = ((a & 0xFF0000) >> 16) as u8;
    vec![op_code, l, h, b]
}

fn assemble_args_1_1(
    op_code: u8,
    arg1: &Expression,
    arg2: &Expression,
    label_values: &HashMap<&str, isize>,
) -> Vec<u8> {
    let a1 = resolve_u8(arg1, label_values);
    let a2 = resolve_u8(arg2, label_values);
    vec![op_code, a1, a2]
}

fn assemble_relative(
    op_code: u8,
    target: &str,
    label_values: &HashMap<&str, isize>,
    current_pos: isize,
) -> Vec<u8> {
    let offset = (current_pos + 2 - *label_values.get(target).unwrap()) as u8;
    vec![op_code, offset]
}

fn assemble_relative_long(
    op_code: u8,
    target: &str,
    label_values: &HashMap<&str, isize>,
    current_pos: isize,
) -> Vec<u8> {
    let offset = (current_pos + 3 - *label_values.get(target).unwrap()) as u16;
    let l = (offset & 0x00FF) as u8;
    let h = ((offset & 0xFF00) >> 8) as u8;
    vec![op_code, l, h]
}

fn assemble_instruction(
    inst: &Instruction,
    label_values: &HashMap<&str, isize>,
    current_pos: isize,
) -> Vec<u8> {
    match inst {
        Instruction::AdcImmediate(value) => assemble_arg_2(0x69, value, label_values),
        Instruction::AdcAbsolute(addr) => assemble_arg_2(0x6D, addr, label_values),
        Instruction::AdcAbsoluteLong(addr) => assemble_arg_3(0x6F, addr, label_values),
        Instruction::AdcDirectPage(addr) => assemble_arg_1(0x65, addr, label_values),
        Instruction::AdcDirectPageIndirect(addr) => assemble_arg_1(0x72, addr, label_values),
        Instruction::AdcDirectPageIndirectLong(addr) => assemble_arg_1(0x67, addr, label_values),
        Instruction::AdcAbsoluteIndexedX(addr) => assemble_arg_2(0x7D, addr, label_values),
        Instruction::AdcAbsoluteLongIndexedX(addr) => assemble_arg_3(0x7F, addr, label_values),
        Instruction::AdcAbsoluteIndexedY(addr) => assemble_arg_2(0x79, addr, label_values),
        Instruction::AdcDirectPageIndexedX(addr) => assemble_arg_1(0x75, addr, label_values),
        Instruction::AdcDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0x61, addr, label_values)
        }
        Instruction::AdcDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0x71, addr, label_values)
        }
        Instruction::AdcDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0x77, addr, label_values)
        }
        Instruction::AdcStackRelative(offset) => assemble_arg_1(0x63, offset, label_values),
        Instruction::AdcStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0x73, offset, label_values)
        }

        Instruction::AndImmediate(value) => assemble_arg_2(0x29, value, label_values),
        Instruction::AndAbsolute(addr) => assemble_arg_2(0x2D, addr, label_values),
        Instruction::AndAbsoluteLong(addr) => assemble_arg_3(0x2F, addr, label_values),
        Instruction::AndDirectPage(addr) => assemble_arg_1(0x25, addr, label_values),
        Instruction::AndDirectPageIndirect(addr) => assemble_arg_1(0x32, addr, label_values),
        Instruction::AndDirectPageIndirectLong(addr) => assemble_arg_1(0x27, addr, label_values),
        Instruction::AndAbsoluteIndexedX(addr) => assemble_arg_2(0x3D, addr, label_values),
        Instruction::AndAbsoluteLongIndexedX(addr) => assemble_arg_3(0x3F, addr, label_values),
        Instruction::AndAbsoluteIndexedY(addr) => assemble_arg_2(0x39, addr, label_values),
        Instruction::AndDirectPageIndexedX(addr) => assemble_arg_1(0x35, addr, label_values),
        Instruction::AndDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0x21, addr, label_values)
        }
        Instruction::AndDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0x31, addr, label_values)
        }
        Instruction::AndDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0x37, addr, label_values)
        }
        Instruction::AndStackRelative(offset) => assemble_arg_1(0x23, offset, label_values),
        Instruction::AndStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0x33, offset, label_values)
        }

        Instruction::AslAccumulator => vec![0x0A],
        Instruction::AslAbsolute(addr) => assemble_arg_2(0x0E, addr, label_values),
        Instruction::AslDirectPage(addr) => assemble_arg_1(0x06, addr, label_values),
        Instruction::AslAbsoluteIndexedX(addr) => assemble_arg_2(0x1E, addr, label_values),
        Instruction::AslDirectPageIndexedX(addr) => assemble_arg_1(0x16, addr, label_values),

        Instruction::BccRelative(target) => {
            assemble_relative(0x90, target, label_values, current_pos)
        }
        Instruction::BcsRelative(target) => {
            assemble_relative(0xB0, target, label_values, current_pos)
        }
        Instruction::BeqRelative(target) => {
            assemble_relative(0xF0, target, label_values, current_pos)
        }
        Instruction::BneRelative(target) => {
            assemble_relative(0xD0, target, label_values, current_pos)
        }
        Instruction::BmiRelative(target) => {
            assemble_relative(0x30, target, label_values, current_pos)
        }
        Instruction::BplRelative(target) => {
            assemble_relative(0x10, target, label_values, current_pos)
        }
        Instruction::BvcRelative(target) => {
            assemble_relative(0x50, target, label_values, current_pos)
        }
        Instruction::BvsRelative(target) => {
            assemble_relative(0x70, target, label_values, current_pos)
        }
        Instruction::BraRelative(target) => {
            assemble_relative(0x80, target, label_values, current_pos)
        }
        Instruction::BrlRelative(target) => {
            assemble_relative_long(0x82, target, label_values, current_pos)
        }

        Instruction::BitImmediate(value) => assemble_arg_2(0x89, value, label_values),
        Instruction::BitAbsolute(addr) => assemble_arg_2(0x2C, addr, label_values),
        Instruction::BitDirectPage(addr) => assemble_arg_1(0x24, addr, label_values),
        Instruction::BitAbsoluteIndexedX(addr) => assemble_arg_2(0x3C, addr, label_values),
        Instruction::BitDirectPageIndexedX(addr) => assemble_arg_1(0x34, addr, label_values),

        Instruction::BrkImplied => vec![0x00],
        Instruction::CopImplied => vec![0x02],

        Instruction::ClcImplied => vec![0x18],
        Instruction::CliImplied => vec![0x58],
        Instruction::CldImplied => vec![0xD8],
        Instruction::ClvImplied => vec![0xB8],

        Instruction::CmpImmediate(value) => assemble_arg_2(0xC9, value, label_values),
        Instruction::CmpAbsolute(addr) => assemble_arg_2(0xCD, addr, label_values),
        Instruction::CmpAbsoluteLong(addr) => assemble_arg_3(0xCF, addr, label_values),
        Instruction::CmpDirectPage(addr) => assemble_arg_1(0xC5, addr, label_values),
        Instruction::CmpDirectPageIndirect(addr) => assemble_arg_1(0xD2, addr, label_values),
        Instruction::CmpDirectPageIndirectLong(addr) => assemble_arg_1(0xC7, addr, label_values),
        Instruction::CmpAbsoluteIndexedX(addr) => assemble_arg_2(0xDD, addr, label_values),
        Instruction::CmpAbsoluteLongIndexedX(addr) => assemble_arg_3(0xDF, addr, label_values),
        Instruction::CmpAbsoluteIndexedY(addr) => assemble_arg_2(0xD9, addr, label_values),
        Instruction::CmpDirectPageIndexedX(addr) => assemble_arg_1(0xD5, addr, label_values),
        Instruction::CmpDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0xC1, addr, label_values)
        }
        Instruction::CmpDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0xD1, addr, label_values)
        }
        Instruction::CmpDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0xD7, addr, label_values)
        }
        Instruction::CmpStackRelative(offset) => assemble_arg_1(0xC3, offset, label_values),
        Instruction::CmpStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0xD3, offset, label_values)
        }

        Instruction::CpxImmediate(value) => assemble_arg_2(0xE0, value, label_values),
        Instruction::CpxAbsolute(addr) => assemble_arg_2(0xEC, addr, label_values),
        Instruction::CpxDirectPage(addr) => assemble_arg_1(0xE4, addr, label_values),

        Instruction::CpyImmediate(value) => assemble_arg_2(0xC0, value, label_values),
        Instruction::CpyAbsolute(addr) => assemble_arg_2(0xCC, addr, label_values),
        Instruction::CpyDirectPage(addr) => assemble_arg_1(0xC4, addr, label_values),

        Instruction::DecAccumulator => vec![0x3A],
        Instruction::DecAbsolute(addr) => assemble_arg_2(0xCE, addr, label_values),
        Instruction::DecDirectPage(addr) => assemble_arg_1(0xC6, addr, label_values),
        Instruction::DecAbsoluteIndexedX(addr) => assemble_arg_2(0xDE, addr, label_values),
        Instruction::DecDirectPageIndexedX(addr) => assemble_arg_1(0xD6, addr, label_values),

        Instruction::DexImplied => vec![0xCA],
        Instruction::DeyImplied => vec![0x88],

        Instruction::EorImmediate(value) => assemble_arg_2(0x49, value, label_values),
        Instruction::EorAbsolute(addr) => assemble_arg_2(0x4D, addr, label_values),
        Instruction::EorAbsoluteLong(addr) => assemble_arg_3(0x4F, addr, label_values),
        Instruction::EorDirectPage(addr) => assemble_arg_1(0x45, addr, label_values),
        Instruction::EorDirectPageIndirect(addr) => assemble_arg_1(0x52, addr, label_values),
        Instruction::EorDirectPageIndirectLong(addr) => assemble_arg_1(0x47, addr, label_values),
        Instruction::EorAbsoluteIndexedX(addr) => assemble_arg_2(0x5D, addr, label_values),
        Instruction::EorAbsoluteLongIndexedX(addr) => assemble_arg_3(0x5F, addr, label_values),
        Instruction::EorAbsoluteIndexedY(addr) => assemble_arg_2(0x59, addr, label_values),
        Instruction::EorDirectPageIndexedX(addr) => assemble_arg_1(0x55, addr, label_values),
        Instruction::EorDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0x41, addr, label_values)
        }
        Instruction::EorDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0x51, addr, label_values)
        }
        Instruction::EorDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0x57, addr, label_values)
        }
        Instruction::EorStackRelative(offset) => assemble_arg_1(0x43, offset, label_values),
        Instruction::EorStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0x53, offset, label_values)
        }

        Instruction::IncAccumulator => vec![0x1A],
        Instruction::IncAbsolute(addr) => assemble_arg_2(0xEE, addr, label_values),
        Instruction::IncDirectPage(addr) => assemble_arg_1(0xE6, addr, label_values),
        Instruction::IncAbsoluteIndexedX(addr) => assemble_arg_2(0xFE, addr, label_values),
        Instruction::IncDirectPageIndexedX(addr) => assemble_arg_1(0xF6, addr, label_values),

        Instruction::InxImplied => vec![0xE8],
        Instruction::InyImplied => vec![0xC8],

        Instruction::JmpAbsolute(addr) => assemble_arg_2(0x4C, addr, label_values),
        Instruction::JmpAbsoluteIndirect(addr) => assemble_arg_2(0x6C, addr, label_values),
        Instruction::JmpAbsoluteIndexedXIndirect(addr) => assemble_arg_2(0x7C, addr, label_values),
        Instruction::JmlAbsoluteLong(addr) => assemble_arg_3(0x5C, addr, label_values),
        Instruction::JmlAbsoluteIndirectLong(addr) => assemble_arg_2(0xDC, addr, label_values),

        Instruction::JsrAbsolute(addr) => assemble_arg_2(0x20, addr, label_values),
        Instruction::JsrAbsoluteIndexedXIndirect(addr) => assemble_arg_2(0xFC, addr, label_values),
        Instruction::JslAbsoluteLong(addr) => assemble_arg_3(0x22, addr, label_values),

        Instruction::LdaImmediate(value, is_byte) => {
            if *is_byte {
                assemble_arg_1(0xA9, value, label_values)
            } else {
                assemble_arg_2(0xA9, value, label_values)
            }
        }
        Instruction::LdaAbsolute(addr) => assemble_arg_2(0xAD, addr, label_values),
        Instruction::LdaAbsoluteLong(addr) => assemble_arg_3(0xAF, addr, label_values),
        Instruction::LdaDirectPage(addr) => assemble_arg_1(0xA5, addr, label_values),
        Instruction::LdaDirectPageIndirect(addr) => assemble_arg_1(0xB2, addr, label_values),
        Instruction::LdaDirectPageIndirectLong(addr) => assemble_arg_1(0xA7, addr, label_values),
        Instruction::LdaAbsoluteIndexedX(addr) => assemble_arg_2(0xBD, addr, label_values),
        Instruction::LdaAbsoluteLongIndexedX(addr) => assemble_arg_3(0xBF, addr, label_values),
        Instruction::LdaAbsoluteIndexedY(addr) => assemble_arg_2(0xB9, addr, label_values),
        Instruction::LdaDirectPageIndexedX(addr) => assemble_arg_1(0xB5, addr, label_values),
        Instruction::LdaDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0xA1, addr, label_values)
        }
        Instruction::LdaDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0xB1, addr, label_values)
        }
        Instruction::LdaDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0xB7, addr, label_values)
        }
        Instruction::LdaStackRelative(offset) => assemble_arg_1(0xA3, offset, label_values),
        Instruction::LdaStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0xB3, offset, label_values)
        }

        Instruction::LdxImmediate(value) => assemble_arg_2(0xA2, value, label_values),
        Instruction::LdxAbsolute(addr) => assemble_arg_2(0xAE, addr, label_values),
        Instruction::LdxDirectPage(addr) => assemble_arg_1(0xA6, addr, label_values),
        Instruction::LdxAbsoluteIndexedY(addr) => assemble_arg_2(0xBE, addr, label_values),
        Instruction::LdxDirectPageIndexedY(addr) => assemble_arg_1(0xB6, addr, label_values),

        Instruction::LdyImmediate(value) => assemble_arg_2(0xA0, value, label_values),
        Instruction::LdyAbsolute(addr) => assemble_arg_2(0xAC, addr, label_values),
        Instruction::LdyDirectPage(addr) => assemble_arg_1(0xA4, addr, label_values),
        Instruction::LdyAbsoluteIndexedX(addr) => assemble_arg_2(0xBC, addr, label_values),
        Instruction::LdyDirectPageIndexedX(addr) => assemble_arg_1(0xB4, addr, label_values),

        Instruction::LsrAccumulator => vec![0x4A],
        Instruction::LsrAbsolute(addr) => assemble_arg_2(0x4E, addr, label_values),
        Instruction::LsrDirectPage(addr) => assemble_arg_1(0x46, addr, label_values),
        Instruction::LsrAbsoluteIndexedX(addr) => assemble_arg_2(0x5E, addr, label_values),
        Instruction::LsrDirectPageIndexedX(addr) => assemble_arg_1(0x56, addr, label_values),

        Instruction::MvnBlockMove(source_bank, target_bank) => {
            assemble_args_1_1(0x54, source_bank, target_bank, label_values)
        }
        Instruction::MvpBlockMove(source_bank, target_bank) => {
            assemble_args_1_1(0x44, source_bank, target_bank, label_values)
        }

        Instruction::NopImplied => vec![0xEA],

        Instruction::OraImmediate(value) => assemble_arg_2(0x09, value, label_values),
        Instruction::OraAbsolute(addr) => assemble_arg_2(0x0D, addr, label_values),
        Instruction::OraAbsoluteLong(addr) => assemble_arg_3(0x0F, addr, label_values),
        Instruction::OraDirectPage(addr) => assemble_arg_1(0x05, addr, label_values),
        Instruction::OraDirectPageIndirect(addr) => assemble_arg_1(0x12, addr, label_values),
        Instruction::OraDirectPageIndirectLong(addr) => assemble_arg_1(0x07, addr, label_values),
        Instruction::OraAbsoluteIndexedX(addr) => assemble_arg_2(0x1D, addr, label_values),
        Instruction::OraAbsoluteLongIndexedX(addr) => assemble_arg_3(0x1F, addr, label_values),
        Instruction::OraAbsoluteIndexedY(addr) => assemble_arg_2(0x19, addr, label_values),
        Instruction::OraDirectPageIndexedX(addr) => assemble_arg_1(0x15, addr, label_values),
        Instruction::OraDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0x01, addr, label_values)
        }
        Instruction::OraDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0x11, addr, label_values)
        }
        Instruction::OraDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0x17, addr, label_values)
        }
        Instruction::OraStackRelative(offset) => assemble_arg_1(0x03, offset, label_values),
        Instruction::OraStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0x13, offset, label_values)
        }

        Instruction::PeaAbsolute(addr) => assemble_arg_2(0xF4, addr, label_values),
        Instruction::PeiDirectPageIndirect(addr) => assemble_arg_1(0xD4, addr, label_values),
        Instruction::PerRelativeLong(target) => {
            assemble_relative_long(0x62, target, label_values, current_pos)
        }

        Instruction::PhaImplied => vec![0x48],
        Instruction::PhbImplied => vec![0x8B],
        Instruction::PhdImplied => vec![0x0B],
        Instruction::PhkImplied => vec![0x4B],
        Instruction::PhpImplied => vec![0x08],
        Instruction::PhxImplied => vec![0xDA],
        Instruction::PhyImplied => vec![0x5A],

        Instruction::PlaImplied => vec![0x68],
        Instruction::PlbImplied => vec![0xAB],
        Instruction::PldImplied => vec![0x2B],
        Instruction::PlpImplied => vec![0x28],
        Instruction::PlxImplied => vec![0xFA],
        Instruction::PlyImplied => vec![0x7A],

        Instruction::RepImmediate(value) => assemble_arg_1(0xC2, value, label_values),

        Instruction::RolAccumulator => vec![0x2A],
        Instruction::RolAbsolute(addr) => assemble_arg_2(0x2E, addr, label_values),
        Instruction::RolDirectPage(addr) => assemble_arg_1(0x26, addr, label_values),
        Instruction::RolAbsoluteIndexedX(addr) => assemble_arg_2(0x3E, addr, label_values),
        Instruction::RolDirectPageIndexedX(addr) => assemble_arg_1(0x36, addr, label_values),

        Instruction::RorAccumulator => vec![0x6A],
        Instruction::RorAbsolute(addr) => assemble_arg_2(0x6E, addr, label_values),
        Instruction::RorDirectPage(addr) => assemble_arg_1(0x66, addr, label_values),
        Instruction::RorAbsoluteIndexedX(addr) => assemble_arg_2(0x7E, addr, label_values),
        Instruction::RorDirectPageIndexedX(addr) => assemble_arg_1(0x76, addr, label_values),

        Instruction::RtiImplied => vec![0x40],
        Instruction::RtsImplied => vec![0x60],
        Instruction::RtlImplied => vec![0x6B],

        Instruction::SbcImmediate(value) => assemble_arg_2(0xE9, value, label_values),
        Instruction::SbcAbsolute(addr) => assemble_arg_2(0xED, addr, label_values),
        Instruction::SbcAbsoluteLong(addr) => assemble_arg_3(0xEF, addr, label_values),
        Instruction::SbcDirectPage(addr) => assemble_arg_1(0xE5, addr, label_values),
        Instruction::SbcDirectPageIndirect(addr) => assemble_arg_1(0xF2, addr, label_values),
        Instruction::SbcDirectPageIndirectLong(addr) => assemble_arg_1(0xE7, addr, label_values),
        Instruction::SbcAbsoluteIndexedX(addr) => assemble_arg_2(0xFD, addr, label_values),
        Instruction::SbcAbsoluteLongIndexedX(addr) => assemble_arg_3(0xFF, addr, label_values),
        Instruction::SbcAbsoluteIndexedY(addr) => assemble_arg_2(0xF9, addr, label_values),
        Instruction::SbcDirectPageIndexedX(addr) => assemble_arg_1(0xF5, addr, label_values),
        Instruction::SbcDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0xE1, addr, label_values)
        }
        Instruction::SbcDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0xF1, addr, label_values)
        }
        Instruction::SbcDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0xF7, addr, label_values)
        }
        Instruction::SbcStackRelative(offset) => assemble_arg_1(0xE3, offset, label_values),
        Instruction::SbcStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0xF3, offset, label_values)
        }

        Instruction::SecImplied => vec![0x38],
        Instruction::SeiImplied => vec![0x78],
        Instruction::SedImplied => vec![0xF8],

        Instruction::SepImmediate(value) => assemble_arg_1(0xE2, value, label_values),

        Instruction::StaAbsolute(addr) => assemble_arg_2(0x8D, addr, label_values),
        Instruction::StaAbsoluteLong(addr) => assemble_arg_3(0x8F, addr, label_values),
        Instruction::StaDirectPage(addr) => assemble_arg_1(0x85, addr, label_values),
        Instruction::StaDirectPageIndirect(addr) => assemble_arg_1(0x92, addr, label_values),
        Instruction::StaDirectPageIndirectLong(addr) => assemble_arg_1(0x87, addr, label_values),
        Instruction::StaAbsoluteIndexedX(addr) => assemble_arg_2(0x9D, addr, label_values),
        Instruction::StaAbsoluteLongIndexedX(addr) => assemble_arg_3(0x9F, addr, label_values),
        Instruction::StaAbsoluteIndexedY(addr) => assemble_arg_2(0x99, addr, label_values),
        Instruction::StaDirectPageIndexedX(addr) => assemble_arg_1(0x95, addr, label_values),
        Instruction::StaDirectPageIndexedXIndirect(addr) => {
            assemble_arg_1(0x81, addr, label_values)
        }
        Instruction::StaDirectPageIndirectIndexedY(addr) => {
            assemble_arg_1(0x91, addr, label_values)
        }
        Instruction::StaDirectPageIndirectLongIndexedY(addr) => {
            assemble_arg_1(0x97, addr, label_values)
        }
        Instruction::StaStackRelative(offset) => assemble_arg_1(0x83, offset, label_values),
        Instruction::StaStackRelativeIndirectIndexedY(offset) => {
            assemble_arg_1(0x93, offset, label_values)
        }

        Instruction::StpImplied => vec![0xDB],

        Instruction::StxAbsolute(addr) => assemble_arg_2(0x8E, addr, label_values),
        Instruction::StxDirectPage(addr) => assemble_arg_1(0x86, addr, label_values),
        Instruction::StxDirectPageIndexedY(addr) => assemble_arg_1(0x96, addr, label_values),

        Instruction::StyAbsolute(addr) => assemble_arg_2(0x8C, addr, label_values),
        Instruction::StyDirectPage(addr) => assemble_arg_1(0x84, addr, label_values),
        Instruction::StyDirectPageIndexedX(addr) => assemble_arg_1(0x94, addr, label_values),

        Instruction::StzAbsolute(addr) => assemble_arg_2(0x9C, addr, label_values),
        Instruction::StzDirectPage(addr) => assemble_arg_1(0x64, addr, label_values),
        Instruction::StzAbsoluteIndexedX(addr) => assemble_arg_2(0x9E, addr, label_values),
        Instruction::StzDirectPageIndexedX(addr) => assemble_arg_1(0x74, addr, label_values),

        Instruction::TaxImplied => vec![0xAA],
        Instruction::TayImplied => vec![0xA8],
        Instruction::TcdImplied => vec![0x5B],
        Instruction::TcsImplied => vec![0x1B],
        Instruction::TdcImplied => vec![0x7B],
        Instruction::TscImplied => vec![0x3B],
        Instruction::TsxImplied => vec![0xBA],
        Instruction::TxaImplied => vec![0x8A],
        Instruction::TxsImplied => vec![0x9A],
        Instruction::TxyImplied => vec![0x9B],
        Instruction::TyaImplied => vec![0x98],
        Instruction::TyxImplied => vec![0xBB],

        Instruction::TrbAbsolute(addr) => assemble_arg_2(0x1C, addr, label_values),
        Instruction::TrbDirectPage(addr) => assemble_arg_1(0x14, addr, label_values),
        Instruction::TsbAbsolute(addr) => assemble_arg_2(0x0C, addr, label_values),
        Instruction::TsbDirectPage(addr) => assemble_arg_1(0x04, addr, label_values),

        Instruction::WaiImplied => vec![0xCB],

        Instruction::XbaImplied => vec![0xEB],
        Instruction::XceImplied => vec![0xFB],
    }
}

fn assemble_directive(dir: &Directive, label_values: &HashMap<&str, isize>) -> Vec<u8> {
    match dir {
        Directive::Origin(_) => Vec::new(),
        Directive::StoreByte(bytes) => bytes
            .0
            .iter()
            .map(|expr| resolve_u8(expr, label_values))
            .collect(),
        Directive::StoreWord(words) => words
            .0
            .iter()
            .flat_map(|expr| {
                let value = resolve_u16(expr, label_values);
                let l = (value & 0x00FF) as u8;
                let h = ((value & 0xFF00) >> 8) as u8;
                [l, h]
            })
            .collect(),
        Directive::AsciiString(s, null_terminated) => {
            let mut result: Vec<u8> = s.bytes().collect();
            if *null_terminated {
                result.push(0);
            }
            result
        }
        Directive::UnicodeString(s, null_terminated) => {
            let mut result: Vec<u8> = s
                .chars()
                .flat_map(|c| {
                    let mut buffer: [u16; 2] = [0; 2];
                    let code_points = c.encode_utf16(&mut buffer);
                    let bytes: Vec<u8> = code_points
                        .iter()
                        .flat_map(|cp| {
                            let l = (cp & 0x00FF) as u8;
                            let h = ((cp & 0xFF00) >> 8) as u8;
                            [l, h]
                        })
                        .collect();
                    bytes
                })
                .collect();
            if *null_terminated {
                result.push(0);
                result.push(0);
            }
            result
        }
    }
}

#[derive(Debug)]
struct Segment {
    offset: isize,
    content: Vec<u8>,
}
impl Segment {
    fn new(offset: isize) -> Self {
        Self {
            offset,
            content: Vec::new(),
        }
    }
}

pub fn assemble_program(prog: &Program) -> DisplayableVec<u8> {
    let label_values = assign_label_values(prog);

    let mut segments: Vec<Segment> = Vec::new();
    let mut current_segment = Segment::new(0);

    let mut current_pos: isize = 0;
    for stm in prog.0.iter() {
        match stm {
            Statement::Instruction(inst) => {
                let mut assembly = assemble_instruction(inst, &label_values, current_pos);
                current_pos += assembly.len() as isize;
                current_segment.content.append(&mut assembly);
            }
            Statement::Directive(dir) => {
                if let Directive::Origin(expr) = dir {
                    current_pos = resolve_u24(expr, &label_values) as isize;
                    segments.push(current_segment);
                    current_segment = Segment::new(current_pos);
                }

                let mut assembly = assemble_directive(dir, &label_values);
                current_pos += assembly.len() as isize;
                current_segment.content.append(&mut assembly);
            }
            _ => {}
        }
    }

    segments.push(current_segment);

    let mut min_addr = usize::MAX;
    let mut max_addr = usize::MIN;
    for segment in segments.iter() {
        if segment.content.len() > 0 {
            let min = segment.offset as usize;
            let max = segment.offset as usize + segment.content.len();

            if min < min_addr {
                min_addr = min;
            }
            if max > max_addr {
                max_addr = max;
            }
        }
    }

    let mut result_assembly: Vec<u8> = vec![0; (max_addr - min_addr) as usize]; //Vec::with_capacity((max_addr - min_addr) as usize);
    for segment in segments.iter() {
        if segment.content.len() > 0 {
            let start = segment.offset as usize - min_addr;
            let end = start + segment.content.len();
            let target = &mut result_assembly[start..end];
            target.copy_from_slice(&segment.content);
        }
    }
    DisplayableVec(result_assembly)
}
