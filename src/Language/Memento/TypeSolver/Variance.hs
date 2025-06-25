{-

Solver for variances with recursion

e.g.
data List [
  Cons<T>(a : T, tail : List<T>) : List<T>,
  Nil : List<T>
]

-- Variance:
List<T> = List<out T>

INPUT : equations of variances, which possibly contain variables
(expression : )
expr = Variance var | + (expr, expr, ... , expr) | * (expr, expr, ... , expr)
var = Covariant | Contravariant | Invariant | Bivariant
equations = [expr1 = expr, expr = expr, ...]

where
-- combination (variable appears a few times in different positions)
(+) x y
  | x == Bivariant = y
  | y == Bivariant = x
  | x == Covariant && y == Covariant = Covariant
  | x == Contravariant && y == Contravariant = Contravariant
  | otherwise = Invariant -- One side is Invariant, or permutation of [Covariant, Contravairant]

unit of (+) is Bivariant

-- composition (variable appears in a nested position (such as function argument, or another type constructor's argument))
(*) x y
  | x == Bivariant || y == Bivariant = Bivariant
  | x == Covariant = y
  | y == Covariant = x
  | x == Contravariant = flipVariance y
  | y == Contravariant = flipVariance x
  | otherwise = Invariant -- If both are invariant, result is invariant

unit of (*) is Covariant

OUTPUT : Valid assignment of variances to variables

ALGORITHM :

-}
module Language.Memento.TypeSolver.Variance where
