# Assumptions

```
f<T where T extends number> : (x : T) => number
  := (x : T) => x;
```

Exsistential Type 的なものをハンドルするときに必要

恣意的な例 :

```
data Ty [
  Cons<T> : (x : T | bool) => Ty<T | string>
];
```

↓

```
switch (x) [
  (Cons(x) : Ty<a>) => {
    // x is (U | bool) where U is a arbitrary type s.t. (U | string) = a
    // ここで内部的にはジェネリクス U を導入し、その型の境界 a <: U | string <: a を導入する (これをどう U に関しての bound として読み替えるんですか？？？)
  },
]
```

assumption の変形

a <: U | c <: b
↓ 広く取る分には問題ない (false-positive が発生するが、型安全側に倒れる)
never <: U <: b

a <: U & c <: b
↓
a <: U <: unknown

(a => b) <: (U => c)
↕
U <: a, b <: c

```
F<a, b> <: F<c, d>
↕
???
```
