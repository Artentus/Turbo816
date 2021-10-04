# About

Turbo816 is a compiled assembly language designed for the 65C816 microprocessor.  
It is higher level than the 65C816s native assembly language, offering constructs like functions,  
but still low level enough as to not come with the kind of performance impact that for example C would.

Included in the repository is a Visual Studio Code extension that enables syntax highlighting on .t816 files.


# Documentation

## Registers

| Name         | Size   | Description                 |
| ------------ | ------ | --------------------------- |
| `R0-R99`     | 16 bit | Freely Assignable Registers |
| `ARG0-ARG15` | 16 bit | Argument Registers          |
| `RET0-RET9`  | 16 bit | Result Registers            |
| `ERR0/ERR1`  | 16 bit | Error Code Registers        |
| `X/Y`        | 16 bit | Index Registers             |
| `SP`         | 16 bit | Stack Pointer               |
| `FL`         | 8 bit  | Status Flags                |
| `DB`         | 8 bit  | Data Bank                   |


## Status Flags

| Name | Bit | Description      |
| ---- | --- | ---------------- |
| `N`  | 7   | Negative         |
| `V`  | 6   | Overflow         |
| `D`  | 3   | BCD Enable       |
| `I`  | 2   | Interrupt Enable |
| `Z`  | 1   | Zero             |
| `C`  | 0   | Carry            |


## Assembler Concepts

| Name                     | Specifier | Definition                                                   | Description                                                        |
| ------------------------ | --------- | ------------------------------------------------------------ | ------------------------------------------------------------------ |
| Identifier               | `{ident}` | `[_a-zA-Z][_0-9a-zA-Z]*`                                     | A name unique to the current scope                                 |
| Label                    | `{label}` | `@{ident}`                                                   | Takes on the value of the current ROM address when parsed          |
| Char                     | `{char}`  |                                                              | A single unicode char in single quotes (supports escape sequences) |
| String                   | `{str}`   |                                                              | A unicode string in double quotes (supports escape sequences)      |
| Expression               | `{expr}`  | `{ident} or {label} or {char} or {expr}`                     | A mathematical expression evaluating to a number                   |
| Immediate Value          | `{imm}`   | `#{expr}`                                                    | An immediate value used by an instruction                          |
| General Purpose Register | `{gpr}`   | `R0-R99 or ARG0-ARG15 or RET0-RET9 or ERR0/ERR1 or ${ident}` | Any general purpose register                                       |
| Index Register           | `{index}` | `X or Y`                                                     | An index register                                                  |
| Register                 | `{reg}`   | `{gpr} or {index} or SP or FL or DB`                         | Any register                                                       |
| Parameter                | `{param}` | `{gpr} or {index} or {expr}`                                 | A function parameter                                               |


## Assembler Directives

| Directive                       | Description                                                                   |
| ------------------------------- | ----------------------------------------------------------------------------- |
| `.org {expr}`                   | Sets the current ROM address                                                  |
| `.def {ident} = {expr}`         | Defines a constant                                                            |
| `.reg {ident} = {gpr}`          | Defines an alias for a General Purpose Register                               |
| `.byte {expr}, ...`             | Stores a number of bytes at the current ROM address                           |
| `.word {expr}, ...`             | Stores a number of words at the current ROM address                           |
| `.ascii {str}`                  | Stores a string in ASCII encoding at the current ROM address                  |
| `.asciiz {str}`                 | Stores a null-terminated string in ASCII encoding at the current ROM address  |
| `.unicode {str}`                | Stores a string in UTF-16 encoding at the current ROM address                 |
| `.unicodez {str}`               | Stores a null-terminated string in UTF-16 encoding at the current ROM address |
| `.call {ident}([{param}, ...])` | Calls a function with optional parameters                                     |
| `.return`                       | Returns from the current function                                             |
| `.fn {ident}([{ident}, ...])`   | Defines a function with optional parameters                                   |
| `.endfn`                        | Ends a function definition                                                    |
| `.main`                         | Defines the entry point of the program (can only be defined once)             |
| `.endmain`                      | Ends the entry point definition                                               |
| `.irq`                          | Defines the IRQ handler of the program (can only be defined once)             |
| `.endirq`                       | Ends the IRQ handler definition                                               |
| `.nmi`                          | Defines the NMI handler of the program (can only be defined once)             |
| `.endnmi`                       | Ends the NMI handler definition                                               |
| `.brk`                          | Defines the BRK handler of the program (can only be defined once)             |
| `.endbrk`                       | Ends the BRK handler definition                                               |
| `.include {str}`                | Includes the specified file into the program at the current ROM address       |
| `// ...`                        | Comment                                                                       |


## Instructions

| Mnemonic         |
| ---------------- |
| `TR`             |
| `LDB`            |
| `STB`            |
| `LD`             |
| `ST`             |
| `PUSH`           |
| `POP`            |
| `INC`            |
| `DEC`            |
| `ADD`            |
| `ADDC`           |
| `SUB`            |
| `SUBB`           |
| `AND`            |
| `XOR`            |
| `OR`             |
| `NEG`            |
| `NOT`            |
| `SHL`            |
| `SHR`            |
| `ROL`            |
| `ROR`            |
| `JPEQ/JPZ/JPF`   |
| `JPNEQ/JPNZ/JPT` |
| `JPG`            |
| `JPL/JPNEG`      |
| `JPGEQ/JPPOS`    |
| `JPLEQ`          |
| `JPC`            |
| `JPCC`           |
| `JPO`            |
| `JPNO`           |
| `JP`             |
| `CMP`            |
| `SSF`            |
| `RSF`            |
| `BIT`            |
| `TEST`           |
| `BRK`            |
| `WAITI`          |


## Code Example

```x86asm
.def E_OK = 0
.def E_INVALID_ARG = 1

@DIGIT:
.ascii "0123456789ABCDEF"

.fn digit_to_char(digit)
    cmp $digit, #0x000F
    jpg @invalid
        ld err0 <- #E_OK

        push x
            tr $digit -> x
            ldb ret0 <- @DIGIT:x
        pop x

        .return
    @invalid:
        ld err0 <- #E_INVALID_ARG
.endfn
```
