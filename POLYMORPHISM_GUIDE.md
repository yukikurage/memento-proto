# Memento Polymorphism Guide

## ✅ Working Features

### 1. Polymorphic Functions
```memento
// Single type parameter
val identity<T> : (x : T) => T := (x : T) => x;

// Multiple type parameters  
val const_value<A, B> : (x : A) => B := (x : A) => some_default_b;
```

### 2. Polymorphic Data Types
```memento
// Option type
data Some<T> : (value : T) => Option;
data None<T> : () => Option;

// Pair type
data Pair<A, B> : (first : A, second : B) => Pair;

// List type
data Cons<T> : (head : T, tail : List) => List;
data Nil<T> : () => List;
```

### 3. Automatic Type Inference
```memento
// Type parameters inferred automatically
val num_result : number := identity(42);        // T inferred as number
val str_result : string := identity("hello");   // T inferred as string

// Works with complex expressions
val my_list : List := Cons(1, Cons(2, Nil())); // T inferred as number
```

### 4. Type Environment Output
The compiler correctly shows polymorphic types:
- `identity`: `TypeScheme ["T"] (TFunction (TGeneric "T") (TGeneric "T"))`
- `Pair`: `TypeScheme ["A","B"] (TFunction (TGeneric "A") (TFunction (TGeneric "B") ...))`

## ❌ Not Yet Supported

### Explicit Type Application Syntax
```memento
// This syntax is not implemented:
val result := identity<number>(42);

// Use type inference instead:
val result : number := identity(42);
```

## Syntax Rules

### Data Declarations
- ✅ `data Constructor<T> : (param : T) => ReturnType;`
- ❌ `data Constructor<T> : T -> ReturnType;` (arrow syntax not supported)

### Function Types
- ✅ `(param : Type) => ReturnType`
- ✅ `(p1 : T1, p2 : T2) => ReturnType` 
- ❌ Complex nested function types may have parsing limitations

### Type Parameters
- ✅ `<T>` single parameter
- ✅ `<A, B, C>` multiple parameters
- ✅ Works in both function and data declarations

## Complete Working Example

```memento
// Polymorphic functions
val identity<T> : (x : T) => T := (x : T) => x;
val const_42<T> : (x : T) => number := (x : T) => 42;

// Polymorphic data types
data Some<T> : (value : T) => Option;
data None<T> : () => Option;
data Pair<A, B> : (first : A, second : B) => Pair;

// Usage with type inference
val test_number : number := identity(42);
val test_string : string := identity("hello");
val some_value : Option := Some(100);
val my_pair : Pair := Pair(42, "test");

val main : () => number := () => test_number;
```

This compiles successfully and produces the correct polymorphic type schemes!