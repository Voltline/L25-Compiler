<div align="center">
  <img src=others/banner.png  alt="">
</div>

# L25-Compiler
> Compiler Principle Final Homework with L25 Language.

* L25 Language is founded by ECNU Compiler Principle Course Team.
* The compiler uses L25 grammar with Flex, Bison and LLVM.
* The compiler is open-source based on MIT License.

## Grammar
> Extended L25 Grammar with EBNF description
```
<program> = "program" <ident> "{" <func_def_list> "main" "{" <stmt_list> "}" "}"

<func_def_list> = <func_def> { <func_def> }

<func_def> = "func" <ident> "(" [ <param_list> ] ")" "{" <stmt_list> "return" <expr> ";" "}"

<param_list> = <typed_ident> { "," <typed_ident> }

<typed_ident> = <ident> [ ":" <type_info> ]

<stmt_list> = <stmt> ";" { <stmt> ";" }

<stmt> = <declare_stmt> | <assign_stmt> | <if_stmt> | <while_stmt> | <input_stmt> |
<output_stmt> | <func_call> | <nested_func_stmt>

<declare_stmt> = "let" <ident>                          // Only declaration, default int
               | "let" <ident> "=" <expr>               // Implicit int, with initial value
               | "let" <ident> ":" "int"                // Explicit int, with default value 0
               | "let" <ident> ":" "int" "=" <expr>     // Explicit int, with initial value
               | "let" <ident> ":" "[" <dim_list> "]"   // Explicit array, default 0 for every slot

<assign_stmt> = (<ident> | <array_subscript_expr>) "=" <expr>
<if_stmt> = "if" "(" <bool_expr> ")" "{" <stmt_list> "}" [ "else" "{" <stmt_list> "}" ]
<while_stmt> = "while" "(" <bool_expr> ")" "{" <stmt_list> "}"
<func_call> = <ident> "(" [ <arg_list> ] ")"
<arg_list> = <expr> { "," <expr> }
<input_stmt> = "input" "(" <input_arg_list> ")"
<output_stmt> = "output" "(" <arg_list> ")"
<nested_func_stmt> = <func_def>

<bool_expr> = <expr> ("==" | "!=" | "<" | "<=" | ">" | ">=") <expr>

<expr> = [ "+" | "-" ] <term> { ("+" | "-") <term> }
<term> = <factor> { ("*" | "/") <factor> }
<factor> = <ident> | <number> | "(" <expr> ")" | <func_call> | <array_subscript_expr>

<array_subscript_expr> = <ident> "[" <array_subscript_list> "]"
<array_subscript_list> = <expr> { "," <expr> }

<dim_list> = <number> {"," <number> }
<type_info> = "int" | "[" <dim_list> "]"

<input_arg_list> = <ident> { "," <ident> }

<ident> = <letter> { <letter> | <digit> }
<number> = <digit> { <digit> }
<letter> = "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
<digit> = "0" | "1" | ... | "9"
```

## Language Features
> A series of extensions to the original L25 grammar have now been implemented.

* Nested Functions within Functions:
```L25
func f1(a) {
    func f2(b) {
        let c = b + 10;
        return c;
    }
    let d = a + f2(a) + 20;
    return d;
}
```
* Definition and Invocation of Multidimensional Arrays:
```L25
... 
main {
    let a: [2, 3, 4];
    let d = a[0, 0, 1] + 10;
}
```
* Postponed Type Declarations(Type Annotations Placed after Identifiers):
```L25
let a: int;
let b: int = 10;
let c = 20;
let d: [3, 4];
```

## Usage
> Compile and run `.l25` source files with flexible options.

### Basic Usage

```bash
./l25cc <source.l25> [options]
```
### Options

| Option           | Description                                      |
|------------------|--------------------------------------------------|
| `-emit-ast`      | Print the AST (Abstract Syntax Tree)             |
| `-emit-scope`    | Print the scope tree after semantic analysis     |
| `-emit-ir`       | Output LLVM IR (`.ll` file)                      |
| `-emit-bc`       | Output LLVM Bitcode (`.bc` file)                 |
| `-o <file>`      | Specify the output filename                      |
| `-help`, `--help`| Show this help message                           |

### Smart Input File Detection

You don't have to specify the source file as the first argument.  
The compiler will **automatically detect the first valid `.l25` file** among the inputs:

```bash
./l25cc -emit-ir test/test1.l25 -o out.ll
./l25cc -o result test/test5.l25 -emit-bc
./l25cc --help
```

### Output Behavior

- If `-emit-ir` or `-emit-bc` is specified (or output filename ends with `.ll` / `.bc`), the compiler will **generate LLVM files** accordingly.
- If neither is specified, it will try to **produce an executable** (requires `clang` and `llvm-as` in `PATH`).

## Project Structure
```
L25-Compiler/
├── ast.cpp
├── include
│   ├── ast.h
│   ├── semanticAnalysis.h
│   └── symbol.h
├── lexer.l
├── LICENSE
├── main.cpp
├── Makefile
├── others
│   ├── banner.png
│   ├── logo-light.png
│   └── logo.png
├── parser.y
├── README.md
├── semanticAnalysis.cpp
├── symbol.cpp
└── test
    ├── test1.l25
    ├── test2.l25
    ├── test3.l25
    ├── test4.l25
    ├── test5.l25
    ├── test6.l25
    └── test7.l25
```

## Build Instructions
### Dependencies
* LLVM (version >= 16)
* Flex (version == 2.6.4)
* Bison (version == 3.8.2)
* Makefile
* Clang (version >= 18.0, with C++20 support)
### Build
* Just use `make` to compile the project, remember to configure your own llvm path in Makefile
```bash
make
./l25cc --help
```