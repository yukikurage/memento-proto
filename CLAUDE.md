# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Memento is an experimental functional programming language compiler written in Haskell. It compiles `.mmt` source files to JavaScript, featuring:

- Functional paradigm with lambda calculus foundation
- Advanced type inference with constraint-based type solver
- Parametric polymorphism with automatic type instantiation
- Pattern matching with exhaustivity checking
- Union and intersection types
- Strict evaluation

## NOTE

This project is in active development, and the architecture is evolving. You can remove legacy code or unused features as needed. The focus is on building a more composable and extensible architecture.

## Development Commands

### Building and Running

```bash
# Build the compiler
./build.sh build
# OR
stack build

# Compile a Memento file to JavaScript
./build.sh compile examples/simple_val.mmt
# Output: dist/js/simple_val.js

# Build, compile, and run in sequence
./build.sh examples/simple_val.mmt

# Run the compiled JavaScript
./build.sh run examples/simple_val.mmt
```

### Testing

```bash
stack test
```

## Architecture

### Core Modules Structure

- `src/Language/Memento/`:
  - `Parser/`: Megaparsec-based parser for `.mmt` files
  - `Syntax/`: AST definitions and language syntax structures
  - `TypeChecker/`: Type inference system (currently disabled in Main.hs:26-31)
  - `Codegen/`: JavaScript code generation from AST
  - `Data/`: Higher-kinded data structures (HFunctor, HFix, HFoldable)

### Key Design Patterns

- **Higher-Kinded Data**: Uses HFunctor pattern for extensible AST
- **Type Solver**: New constraint-based type solver with union/intersection types
- **Parser Combinators**: Megaparsec for parsing
- **Type Inference**: Advanced type solver supporting subtyping, unions, intersections, and polymorphism

### Language Syntax Evolution

The language uses a modern functional syntax with type annotations.

## Current Development Status

- **Branch**: `more-composable-architecture`
- **Type Solver**: New constraint-based type solver implemented (see src/Language/Memento/TypeSolver/)
- **Type Checker**: Original type checker disabled, replaced with new type solver
- **Recent Focus**: Polymorphism, pattern matching, and advanced type system features

## Type Solver Architecture

The new type solver implements the algorithm described in `docs/TYPE_SOLVER.md`:

- **Constraint Generation**: Creates subtype constraints from expressions (`ConstraintGen.hs`)
- **Constraint Solving**: Multi-step algorithm with decomposition, contradiction checking, and branch splitting
- **Type Normalization**: Simplifies union/intersection types using algebraic laws
- **Subtyping**: Supports structural subtyping with contravariance for functions
- **AST Integration**: Full integration with Memento's higher-order functor AST structure
- **Variance Solver**: Recursive equation solver for determining variance of type parameters (`VarianceSolver.hs`)
  - Generates variance equations from data type definitions
  - Uses fixed-point iteration to solve recursive variance relationships
  - Correctly handles covariant, contravariant, invariant, and bivariant positions

## Supported Language Features

The constraint generator now supports:

- ✅ **Value declarations**: `val x : number = 42;`
- ✅ **Function definitions**: `(input : number) => input * 2`
- ✅ **Function declaration syntax**: `val add(x : number, y : number) : number = x + y;`
- ✅ **Function application**: `doubler(21)`
- ✅ **Lambda expressions**: `(_ : number) => expression`
- ✅ **Binary operations**: `+`, `-`, `*`, `/`, `==`, `<`, `>`
- ✅ **If expressions**: `if(condition) { then } else { else }`
- ✅ **Let bindings**: `let tmp : number = 2;`
- ✅ **Block expressions**: `{ let x = 1; x + 2 }`
- ✅ **Pattern matching**: `switch (expr) [(pat : type) => result, ...]`
- ✅ **Exhaustivity checking**: Full pattern matrix algorithm with union type support
- ✅ **Polymorphism**: Type parameters with automatic type inference
- ✅ **Data type declarations**: `data Option [Some<T> : (value : T) => Option<T>, None<T> : () => Option<T>];`
- ✅ **Type aliases**: `type MyNumber = number;`

### Data Type Declaration Syntax

Data types are declared using square brackets containing constructor definitions:

```memento
// Single constructor
data Box [MkBox<T> : (value : T) => Box<T>];

// Multiple constructors (comma-separated)
data Option [
  Some<T> : (value : T) => Option<T>,
  None<T> : () => Option<T>
];

// Constructor function syntax sugar
data Result [
  Ok<T, E>(value : T) : Result<T, E>,
  Err<T, E>(error : E) : Result<T, E>
];
```

### Pattern Matching Features

- ✅ **Variable patterns**: `(x : number) =>` - binds variable `x`
- ✅ **Wildcard patterns**: `(_ : number) =>` - matches anything
- ✅ **Literal patterns**: `(42 : number) =>`, `("hello" : string) =>`
- ✅ **Constructor patterns**: `(SomeNum(x) : Maybe) =>`, `(Pair(a, b) : Pair) =>`
- ✅ **Multi-argument constructors**: `Triple(x, y, z)` with proper arity checking
- ✅ **Nested patterns**: `Outer(Inner(Leaf(42)))` with arbitrary depth
- ✅ **Literal patterns in constructors**: `SomeNum(1)`, `Point(0, 0)`

### Pattern Syntax IMPORTANT

- **Zero-argument constructors require parentheses**: `A()` not `A`
  - `(A : Type)` → Variable pattern (binds variable named `A`)
  - `(A() : Type)` → Constructor pattern (matches constructor `A`)
- **Constructor namespace**: Constructor and type names are in separate namespaces
  - A constructor `Some` and a type `Some` can coexist without conflict

## Implementation Notes for Pattern Matching

### Pattern Tree Architecture

- **PatternTree**: Core data structure supporting arbitrary nesting
  - `PTWildcard`: Matches anything
  - `PTVariable`: Matches anything and binds a variable
  - `PTLiteral`: Matches specific literal values
  - `PTConstructor`: Matches constructor with nested patterns
- **Pattern Matrix**: Used for exhaustivity analysis (`PatternMatrix`)
- **Pattern conversion**: `astToPatternTree` converts AST patterns to pattern trees

### Exhaustivity Algorithm

- Based on pattern matrix decomposition (simplified version)
- Handles union types by extracting all possible constructors
- Supports nested pattern exhaustivity checking
- Current limitations:
  - Simplified algorithm - full pattern matrix algorithm not yet implemented
  - Witness generation for missing patterns is basic

### Type Environment Management

- Pattern variables are scoped to their match case
- Uses `saveTypeEnv`/`restoreTypeEnv` for local scoping
- Constructor names stored with `TYPE_` prefix to avoid namespace collisions

### Known Edge Cases

- Empty switch expressions correctly fail with contradictory constraints
- Wrong constructor arity caught with clear error messages
- Unknown constructors properly reported
- Overlapping patterns not detected (first match wins semantics)

## Important Notes

- When modifying the parser, test with examples in `examples/` directory
- JavaScript output goes to `dist/js/` directory
- Full AST type checking enabled in Main.hs with `typeCheckAST`
- Type solver modules: Types, Solver, Subtype, Normalize, ConstraintGen, VarianceSolver, Demo
- Pattern matching examples: `exhaustivity_fail_test.mmt`, `pattern_bounds_test.mmt`
- Variance solver uses fixed-point iteration on a finite lattice (Bivariant < Covariant, Contravariant < Invariant)
- Type constructor information stored in `TypeConstructorInfo` with variance data

## Polymorphism Support

### Type Parameters

- ✅ **Polymorphic functions**: `val identity<T> : (x : T) => T = (x : T) => x;`
- ✅ **Polymorphic data types**: `data Box [MkBox<T> : (value : T) => Box<T>];`
- ✅ **Multi-parameter polymorphism**: `data Pair [MkPair<A, B> : (first : A, second : B) => Pair<A, B>];`
- ✅ **Type inference**: Automatic instantiation of type variables
- ✅ **Type schemes**: Proper quantification with `TypeScheme [vars] type`

### Parser Syntax Rules (CRITICAL)

1. **Function types require parameter names**:

   - ❌ `(A) => B` - INVALID
   - ✅ `(x : A) => B` - VALID
   - ❌ `(f : (B) => C)` - INVALID
   - ✅ `(f : (b : B) => C)` - VALID

2. **No explicit type application syntax**:
   - ❌ `identity<number>(42)` - NOT SUPPORTED
   - ✅ `val x : Box<number> = MkBox(42)` - Type applications allowed in type annotations
   - ✅ `val x : number = identity(42)` - Type parameters inferred in expressions

### Type Solver Integration

- **Generalization**: Polymorphic definitions create type schemes
- **Instantiation**: Fresh type variables generated on use
- **Constraint generation**: Works with generic types (TGeneric)
- **Normalization**: Handles TGeneric and TApplication types
- **Variance Analysis**: Automatic covariant/contravariant/invariant detection for type parameters
- **Recursive Variance Solver**: Fixed-point algorithm for solving variance equations in recursive types

<!-- ### Critical Type Solver Fixes (December 2024)

The type solver had three critical soundness issues that were fixed:

1. **Fixed Unsound Unification Logic**:

   - **Problem**: `([t], []) -> Just t` was unsound - allowed assigning any supertype
   - **Fix**: Only unify when same type appears in both lower and upper bounds
   - **Impact**: Prevents incorrect type assignments and maintains soundness

2. **Fixed Bidirectional Constraint Preservation**:

   - **Problem**: `x <: y` only generated bounds for `x`, losing constraint information
   - **Fix**: Enhanced `calculateBoundsKeepConstraints` to preserve all constraint information
   - **Impact**: Ensures all subtyping relationships are properly tracked

3. **Added Comprehensive Generic Type Support**:
   - **Problem**: `TGeneric` types not handled throughout solver pipeline
   - **Fix**: Added `containsGeneric`, enhanced decomposition, branching, and contradiction checking
   - **Impact**: Enables proper handling of polymorphic types in constraint solving

These fixes ensure the type solver follows the TYPE_SOLVER.md specification correctly and maintains soundness for polymorphic type inference. -->

### Implementation Details

- Type parameters stored in `Definition` AST nodes
- `inferPolyValueDecl` handles polymorphic value definitions
- `inferPolyDataDecl` handles polymorphic data constructors
- Type environment stores `TypeScheme` instead of raw `Type`
- Automatic instantiation in `lookupVar` function

### Examples

```memento
// Working polymorphic code
val compose<A, B, C> : (f : (b : B) => C, g : (a : A) => B, x : A) => C =
  (f : (b : B) => C, g : (a : A) => B, x : A) => f(g(x));

// Data types use square brackets for constructors
data List [
  Cons<T> : (head : T, tail : List<T>) => List<T>,
  Nil<T> : () => List<T>
];

val my_list : List<number> = Cons(1, Cons(2, Nil())); // Type applications in annotations

// Function syntax sugar is also supported
val add(x : number, y : number) : number = x + y;
val identity<T>(x : T) : T = x;
```

### Variance Analysis

The type solver automatically determines variance for type parameters:

```memento
// Covariant: T appears only in positive positions
data Box [
  MkBox<T> : (value : T) => Box<T>
];

// Contravariant: T appears only in negative positions  
data Consumer [
  MkConsumer<T> : (consume : (x : T) => number) => Consumer<T>
];

// Invariant: T appears in both positive and negative positions
data FuncBox [
  MkFuncBox<T> : (f : (x : T) => T) => FuncBox<T>
];

// Recursive types are handled correctly
data List [
  Cons<T> : (head : T, tail : List<T>) => List<T>,  // List is covariant in T
  Nil<T> : () => List<T>
];
```
