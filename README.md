# Memento Programming Language (Prototype)

Memento is an experimental functional programming language featuring advanced type inference, polymorphism, and pattern matching.

## Features

- **Functional Paradigm**: Lambda calculus foundation with strict evaluation
- **Advanced Type System**: Constraint-based type solver with union/intersection types
- **Parametric Polymorphism**: Automatic type instantiation and variance analysis
- **Pattern Matching**: Exhaustivity checking with constructor patterns
- **Separated Constructor/Type Semantics**: Distinct namespaces for constructors and types
- **Effect System**: Tracking side effects (ZeroDiv, Throw)
- **Pipeline Operators**: Intuitive data flow with `|>` and `<|`

## Language Syntax

Memento supports both pipeline-style and modern declaration syntax:

### Modern Syntax (Recommended)

```memento
// Value declarations
val x : number := 42;
val message : string := "hello";

// Function definitions
val increment : (x : number) => number := (x : number) => x + 1;
val double : (x : number) => number := (x : number) => x * 2;

// Polymorphic functions
val identity<T> : (x : T) => T := (x : T) => x;
val compose<A, B, C> : (f : (b : B) => C, g : (a : A) => B, x : A) => C :=
  (f : (b : B) => C, g : (a : A) => B, x : A) => f(g(x));

// Data declarations with separated constructor/type semantics
data Some<T> : (value : T) => Option;
data Cons<T> : (head : T, tail : List) => List;
data Nil : () => List;

// Pattern matching
val process : (opt : Option) => number := (opt : Option) =>
  switch (opt) [
    (Some(x) : Option) => x,
    (None() : Option) => 0
  ];
```

### Pipeline Syntax (Legacy)

```memento
// Value binding
42 |> x : number ->

// Lambda expressions
(x -> x + 1) |> increment ->
5 |> double |> increment ->
```

### Type System

Memento features a sophisticated constraint-based type system:

**Primitive Types:**

- `number`: Numeric type with singleton literal types (`42 : 42`)
- `bool`: Boolean type (`true : true`, `false : false`)
- `string`: String type (`"hello" : "hello"`)

**Advanced Types:**

- **Function Types**: `(param : InputType) => OutputType`
- **Union Types**: `A | B` for alternatives
- **Intersection Types**: `A & B` for combinations
- **Generic Types**: `SomeType<T, U>` with variance analysis
- **Constructor Types**: Separate from type names

**Type Inference:**

```memento
// Explicit type annotation
val x : number := 42;

// Type inference works
val y := 10;  // Inferred as number

// Polymorphic type inference
val result := identity(42);  // T inferred as number
```

### Pattern Matching

Memento supports comprehensive pattern matching with exhaustivity checking:

```memento
// Pattern types
val example : (input : Option) => number := (input : Option) =>
  switch (input) [
    (Some(x) : Option) => x,           // Constructor pattern
    (None() : Option) => 0             // Zero-argument constructor
  ];

// Nested patterns
data Pair<A, B> : (first : A, second : B) => Pair;
val extract : (p : Pair) => number := (p : Pair) =>
  switch (p) [
    (Pair(x, y) : Pair) => x + y
  ];
```

### Separated Constructor/Type Semantics

**Key Innovation**: Constructor names and type names are completely separate:

```memento
// Constructor 'Some' creates values of type 'Option'
data Some<T> : (value : T) => Option;
data None : () => Option;

// Multiple constructors can create the same type
data Cons<T> : (head : T, tail : List) => List;
data Nil : () => List;

// Usage
val my_option : Option := Some(42);      // Constructor: Some, Type: Option
val my_list : List := Cons(1, Nil());    // Constructors: Cons/Nil, Type: List
```

## Build and Usage

### Building the Compiler

```bash
# Build the compiler
stack build
# OR
./build.sh build

# Compile a Memento file to JavaScript
./build.sh compile examples/simple_val.mmt
# Output: dist/js/simple_val.js

# Build, compile, and run in sequence
./build.sh examples/simple_val.mmt

# Run the compiled JavaScript
./build.sh run examples/simple_val.mmt
```

### Example Programs

**Basic Values and Functions:**

```memento
val x : number := 42;
val increment : (n : number) => number := (n : number) => n + 1;
val result : number := increment(x);  // 43
```

**Polymorphic Functions:**

```memento
val identity<T> : (x : T) => T := (x : T) => x;
val test_num : number := identity(42);
val test_str : string := identity("hello");
```

**Data Types and Pattern Matching:**

```memento
data Some<T> : (value : T) => Option;
data None : () => Option;

val unwrap : (opt : Option) => number := (opt : Option) =>
  switch (opt) [
    (Some(x) : Option) => x,
    (None() : Option) => 0
  ];

val example : number := unwrap(Some(42));  // 42
```

## Testing

```bash
# Run the test suite
stack test

# Test edge cases
./build.sh compile examples/edge_cases/simple_poly.mmt
./build.sh compile examples/edge_cases/working_composition.mmt
```

## Architecture

Memento is implemented in Haskell with a modular architecture:

- **Parser**: Megaparsec-based parser for `.mmt` files
- **Type Solver**: Advanced constraint-based type inference system
- **AST**: Higher-kinded data structures with extensible syntax
- **Codegen**: JavaScript code generation from AST
- **Pattern Matching**: Production-level exhaustivity checking

See `CLAUDE.md` for detailed development instructions.

## Current Status

✅ **Completed Features:**

- Separated constructor/type semantics
- Parametric polymorphism with type inference
- Pattern matching with exhaustivity checking
- Singleton literal types (`42 : 42`)
- Union and intersection types
- Variance analysis for type parameters
- JavaScript code generation

🚧 **In Progress:**

- Multi-argument polymorphic function constraint optimization
- Effect system integration
- Library system
