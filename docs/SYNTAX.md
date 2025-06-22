# Memento Language Syntax Specification

## Lexical Structure

### Comments

```
comment ::= "//" <any character except newline>* <newline>
```

### Identifiers

```
identifier ::= <letter> (<letter> | <digit> | "_")*
type_identifier ::= <letter> (<letter> | <digit> | "_")*
```

### Literals

```
number_literal ::= <digit>+ ("." <digit>+)?
bool_literal ::= "true" | "false"
```

## Type Syntax

```
type ::= base_type
       | function_type
       | tuple_type
       | type_identifier

base_type ::= "number" | "bool" | "string" | "never" | "unknown"

function_type ::= "(" param_list ")" "=>" type

tuple_type ::= "[" type ("," type)* "]"

param_list ::= param ("," param)*

param ::= identifier ":" type
```

## Declarations

### Value Declaration

```
val_decl ::= "val" identifier ":" type ":=" expression ";"
```

### Type Declaration

```
type_decl ::= "type" type_identifier ":=" type ";"
```

### Data Declaration

```
data_decl ::= "data" identifier : type_identifier ";"
```

## Expressions

```
expression ::= literal
             | identifier
             | lambda
             | application
             | if_expression
             | switch_expression
             | binary_expression
             | tuple_expression
             | tuple_access
             | block_expression
             | "(" expression ")"

literal ::= number_literal | bool_literal

lambda ::= "(" param_list ")" "=>" expression

application ::= expression "(" arg_list ")"
              | expression "|>" expression
              | expression "<|" expression

if_expression ::= "if" "(" expression ")" "{" expression "}" "else" "{" expression "}"

let_expression ::= "let" identifier ":" type ":=" expression ";"

switch_expression ::= "switch" "(" expression ")" "[" pattern_list "]"

block_expression ::= "{" (let_expression)* expression "}"

tuple_expression ::= "[" expression ("," expression)* "]"

tuple_access ::= expression "[" expression "]"

binary_expression ::= expression binary_op expression

binary_op ::= "+" | "-" | "*" | "/" | "==" | ">" | "<"
```

## Patterns

```
pattern ::= variable_pattern
          | wildcard_pattern
          | literal_pattern
          | constructor_pattern

variable_pattern ::= identifier

wildcard_pattern ::= "_"

literal_pattern ::= number_literal | bool_literal

constructor_pattern ::= type_identifier "(" pattern_args ")"

pattern_args ::= pattern ("," pattern)*

switch_case ::= "(" pattern ":" type ")" "=>" expression

pattern_list ::= switch_case ("," switch_case)*
```

## Programs

```
program ::= declaration*

declaration ::= val_decl
              | type_decl
              | data_decl
```

## Operator Precedence (from highest to lowest)

1. Function application, tuple access `()`, `[]`
2. Pipe operators `|>`, `<|`
3. Multiplicative `*`, `/`
4. Additive `+`, `-`
5. Comparison `==`, `>`, `<`

## Associativity

- Pipe operators (`|>`, `<|`) are left-associative
- Function application is left-associative
- Binary operators are left-associative

## Special Syntax Notes

1. **Main Entry Point**: A program may define `val main : (input : number) => number` as the entry point
2. **Recursive Functions**: Functions can reference themselves directly
