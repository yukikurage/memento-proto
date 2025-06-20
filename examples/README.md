# Memento Language Examples

This directory contains example programs demonstrating various features of the Memento programming language. All examples use the correct modern syntax and have been tested to compile successfully.

## Directory Structure

### `/basics` - Fundamental Language Features (5 examples)
- **`simple_val.mmt`** - Basic value declarations with type annotations ✅
- **`simple_function.mmt`** - Function definitions and type signatures ✅
- **`number_branch.mmt`** - Conditional expressions and let bindings ✅
- **`bool_test.mmt`** - Boolean data types and constructors ✅
- **`literals_and_types.mmt`** - Basic types and pattern matching ✅

### `/polymorphism` - Generic Programming (5 examples)
- **`simple_poly_def.mmt`** - Basic polymorphic function definitions ✅
- **`simple_poly.mmt`** - Polymorphic function application ✅
- **`poly_literals.mmt`** - Polymorphism with literal types ✅
- **`polymorphic_inference.mmt`** - Advanced type inference examples ⚠️ (has type errors but compiles)
- **`working_polymorphic_data.mmt`** - Polymorphic data types (Box example) ✅

### `/patterns` - Pattern Matching (4 examples)
- **`pattern_test.mmt`** - Zero-argument constructor patterns ✅
- **`test_pattern_fix.mmt`** - Pattern matching with data types ✅
- **`test_lazy_pattern.mmt`** - Lazy evaluation patterns ✅
- **`exhaustivity_example.mmt`** - Exhaustiveness checking demonstration ✅

### `/advanced` - Advanced Features (3 examples)
- **`compose_test.mmt`** - Function composition with multiple type parameters ⚠️ (has type errors but compiles)
- **`working_composition.mmt`** - Various composition patterns ✅
- **`constructor_only.mmt`** - Constructor-specific features ✅
- **`new_syntax_test.mmt`** - Multi-constructor data type syntax ✅

## Key Language Features Demonstrated

### 1. Data Types
- **Multi-constructor syntax**: `data TypeName [Constructor1 : ..., Constructor2 : ...]`
- **Polymorphic data types** with type parameters: `data Box [Box<T> : (value : T) => Box]`
- **Zero-argument constructors**: Must use `()` in both definition and pattern matching

### 2. Pattern Matching
- **Switch expressions**: `switch (expr) [(pattern : type) => result, ...]`
- **Constructor patterns**: `(Constructor(args) : Type) =>`
- **Wildcard patterns**: `(_ : Type) =>`
- **Literal patterns**: `(42 : number) =>`, `(true : bool) =>`

### 3. Functions
- **Polymorphic functions**: `val func<T> : (x : T) => T := ...`
- **Lambda expressions**: `(x : Type) => expression`
- **Function application**: `func(arg1, arg2)`

### 4. Type System
- **Type inference** with explicit annotations
- **Parametric polymorphism** with automatic instantiation
- **Constraint-based type solving**

## Syntax Rules (Important!)

1. **Data definitions must use bracket syntax**:
   ```memento
   data TypeName [Constructor : (args) => TypeName];
   ```

2. **Return types cannot have type parameters**:
   ```memento
   data Box<T> : (value : T) => Box;  // ✅ Correct
   data Box<T> : (value : T) => Box<T>; // ❌ Invalid
   ```

3. **Zero-argument constructors require parentheses**:
   ```memento
   data Bool [True : () => Bool];
   val x : Bool := True();  // ✅ Correct
   ```

4. **Unit type must be defined explicitly**:
   ```memento
   data Unit [Unit : () => Unit];
   ```

## Running Examples

To compile and run an example:

```bash
./build.sh examples/basics/simple_val.mmt
```

This will:
1. Compile the `.mmt` file to JavaScript
2. Output to `dist/js/simple_val.js`
3. Execute the generated JavaScript

## Learning Path

1. **Start with basics**: `simple_val.mmt`, `simple_function.mmt`
2. **Learn data types**: `bool_test.mmt`, `literals_and_types.mmt`
3. **Pattern matching**: `pattern_test.mmt`, `test_pattern_fix.mmt`
4. **Polymorphism**: `simple_poly_def.mmt`, `working_polymorphic_data.mmt`
5. **Advanced features**: `new_syntax_test.mmt`, `working_composition.mmt`

## Status Legend
- ✅ Compiles and runs successfully
- ⚠️ Compiles with type errors but generates working code

All examples follow the correct Memento syntax and demonstrate real language features.