# L25-Compiler
> Compiler Principle Final Homework with L25 Language.

* L25 Language is founded by ECNU Compiler Principle Course Team.
* The compiler uses L25 grammar with Flex, Bison and LLVM.
* The compiler is open-source based on MIT License.

## Grammar
> L25 Grammar with EBNF description
```
<program> = "program" <ident> "{" <func_def_list> "main" "{" <stmt_list> "}" "}"

<func_def_list> = <func_def> { <func_def> }

<func_def> = "func" <ident> "(" [ <param_list> ] ")" "{" <stmt_list> "return" <expr> ";" "}"

<param_list> = <ident> { "," <ident> }

<stmt_list> = <stmt> ";" { <stmt> ";" }

<stmt> = <declare_stmt> | <assign_stmt> | <if_stmt> | <while_stmt> | <input_stmt> |
<output_stmt> | <func_call> | <nested_func_stmt>

<declare_stmt> = "let" <ident> [":" <type_info>] [ "=" <expr> ]
<assign_stmt> = <ident> "=" <expr>
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

<array_subscript_expr> = <ident> "[" <dim_list> "]"

<dim_list> = <number> {"," <number> }
<type_info> = "int" | "[" <dim_list> "]"

<input_arg_list> = <ident> { "," <ident> }

<ident> = <letter> { <letter> | <digit> }
<number> = <digit> { <digit> }
<letter> = "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
<digit> = "0" | "1" | ... | "9"
```

## Expansions
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

## Technology
* C++ 17
* LLVM
* Flex
* Bison