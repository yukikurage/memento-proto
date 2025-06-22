# Memento Parser Rules for Polymorphism

## Function Type Syntax

### ❌ INVALID - Anonymous parameters not supported
```memento
// These will NOT parse:
val f : (A) => B := ...
val compose : (f : (B) => C, g : (A) => B) => ...
```

### ✅ VALID - All parameters need names
```memento
// Always use named parameters:
val f : (x : A) => B := ...
val compose : (f : (b : B) => C, g : (a : A) => B) => ...
```

## Data Type Syntax

### ❌ INVALID - Type parameters in return types
```memento
// These will NOT parse:
data Some<T> : (value : T) => Some<T>;
data List<T> : (head : T) => List<T>;
```

### ✅ VALID - Plain type names in return position
```memento
// Return types should be plain names:
data Some<T> : (value : T) => Option;
data Cons<T> : (head : T, tail : List) => List;
```

## Type Usage Syntax

### ❌ NOT SUPPORTED - Explicit type applications
```memento
// These features are NOT implemented:
val x : Some<number> := ...        // Type application in type position
val y := identity<number>(42);     // Explicit type arguments in expressions
```

### ✅ SUPPORTED - Type inference
```memento
// Let type inference do the work:
val x : Option := Some(42);       // T inferred as number
val y : number := identity(42);   // T inferred as number
```

## Complete Valid Example

```memento
// Polymorphic function with proper syntax
val compose<A, B, C> : (f : (b : B) => C, g : (a : A) => B, x : A) => C := 
  (f : (b : B) => C, g : (a : A) => B, x : A) => f(g(x));

// Polymorphic data with proper syntax  
data Some<T> : (value : T) => Option;
data None<T> : () => Option;

// Usage with type inference
val add_one : (x : number) => number := (x : number) => x + 1;
val double : (x : number) => number := (x : number) => x * 2;
val add_then_double : (x : number) => number := compose(double, add_one);
```

## Key Rules Summary

1. **All function parameters must have names** - `(x : T)` not `(T)`
2. **Data return types can't have type parameters** - `=> Option` not `=> Option<T>`
3. **No explicit type application syntax** - Use type inference instead
4. **Multi-arg functions use commas** - `(x : A, y : B) => C`