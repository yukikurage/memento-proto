# Partial Type Solver

## Prerequisites

Including Lit and and/or (representing intersect and union)

S, T = Top | Bottom | T -> T | or (Tn) | and (Tn) | int | num... | 1 | 2 ... | true | false

VS, VT = Top | Bottom | VT -> VT | ... | x (type variable)

for typing.

Assume subtyping computation can only be done with T that doesn't contain type variables

Subtyping computation

or (Tn)

Constraint

VT <: VT

Attempt to solve for sets of these.

## Basic Approach

Receives:

cts : Set [Constraint]

### 1. Decompose contents of cts appropriately, reducing as much as possible to x <: VT or VT <: x form

List of basic decomposition operations:

- VS <: and (VTn)
  → For n, VS <: VT_n
- or (VSn) <: VT
  → For n, VS_n <: VT
- (VS1 -> VS2) <: (VT1 -> VT2)
  → VT1 <: VS1 and VS2 <: VT2

After applying these, we reduce to the following forms (?)

i. Both sides are T ← Happy!
ii. One side is x and the other is VT ← Pretty happy!
iii. Right side is or, left side is not x

- VS <: or (VTn)

iv. Left side is and, right side is not x

- and (VSn) <: VT

v. One side is a func type of VT, and the other side is neither x nor func nor and/or, therefore becomes one of:

- VS1 -> VS2 <: int/num... /0/1 ...
- int/num...0/1/... <: VS1 -> VS2
- VS1 -> VS2 <: Top/Bottom
- Top/Bottom <: VS1 -> VS2

### 2. Contradiction Check for cts

For case i, we can definitively check subtyping (really?)
Same for case v

As a special case of ii, we have x <: x. This doesn't cause contradiction so OK! → Mark as checked

If contradiction occurs, reject the branch as cts is contradictory
Remove checked items from cts

### 3. Boundary Calculation and Boundary Condition Propagation

From previous work, only cases ii, iii, iv remain

Here, use ii (where one side is x) to calculate type boundaries

Collecting all lower and upper bounds:

{lower bound types...} <: x <: {upper bound types...}

Note that since x may still exist in iii, iv and (the non-variable side of ii), satisfying these lower and upper bounds doesn't guarantee type resolution

※ Cases iii, iv are clearly problematic, but for case ii we have examples like:

x <: number
(1 -> number) <: y <: (x -> number)

In this set, if x ~ 1 then OK, but if x ~ number then contradiction

Now, for calculated lower bounds VS_n, VT_n, add new Constraint to cts:

or (VS_m) <: and (VT_n) # normalize might be effective here

(Excluding already added or trivially contradictory ones)
Also, unify x-related constraints to only:
or (VS_m) <: x and

x <: and (VT_n).

### 4. Trivial Assignment and cts Refinement

If the same expression appears in both upper and lower bounds, assign to x.

Repeat 1 ~ 4 until no changes occur

### 5. Branch Splitting → If unable to split, go to 7

The following forms of Constraint remain:

ii. One side is x and the other is VT
iii. Right side is or, left side is not x
iv. Left side is and, right side is not x

Here we decompose ii into two:

ii(a). One side is x and the other is Top, Bottom, variable, 1, 2..., num, int,... (simple types)
ii(b). One side is x and the other is function (note: and/or are eliminated by iii, iv and decomposition)

For iii, iv, ii(b), decompose cts as follows:

#### Case ii(b)

Copy cts to cts1 and cts2

For x <: VS -> VT
x must be Bottom or function type.

- In cts1, assign Bottom to x
- In cts2, assign y -> z to x with new variables y, z

For VS -> VT <: x
x must be Top or function type

- In cts1, assign Top to x
- In cts2, assign y -> z to x with new variables y, z

Delete original Constraint

#### Case iii

We have x <: or (VTn)

Create n cts, each adding constraint x <: VTn

Delete original Constraint

#### Case iv

Same as iii

### 6. Computation of Branched cts

Solve each branch, if a non-contradictory branch is found then no contradiction, if all have contradictions, then contradiction

### 7. Contradiction/Non-contradiction Determination

Only ii(b) remains. That is, reduced to constraints between Top, Bottom, x, lit, prim.

By constraint propagation in 4, if no contradiction at this point, can consider non-contradictory (unproven, needs verification, or counterexample)
→ ChatGPT says it's fine if there are no cycles! But I think cycles disappear in 4, when y <: x <: y occurs, x <= y is assigned

## Appendix

### About Normalize

In various and/or operations, converting to simpler forms produces better results. Perform the following conversions:

VT | Bottom = VT
VT | Top = Top
VT | VT = VT
VT & Bottom = Bottom
VT & Top = VT
VT & VT = VT

T1 | T2 where T1 <: T2 = T2 (since computing subtyping, only for types without variables)
T1 & T2 where T1 <: T2 = T1

Distributive law (lean towards or) (possible performance degradation?) (reverse conversion might be better, though finding common parts seems tedious)

VT1 & (VT2 | VT3) = (VT1 & VT2) | (VT1 & VT3)

Functionalization (increases possibility of using function decomposition laws?)

(VT1 -> VT2) | (VT3 -> VT4) = (VT1 & VT2 -> VT2 | VT4)
(VT1 -> VT2) & (VT3 -> VT4) = (VT1 | VT2 -> VT2 & VT4)

For example, on left side use |, on right side use function (same for &) might be good - want to use decomposition rules as much as possible

Absorption

VT1 & (VT1 | VT2) = VT1

VT1 | (VT1 & VT2) = VT1

Backend is JavaScript, basically

The meaning of (A -> B) | (C -> D) is "function type that is either A -> B or C -> D", and since no type conversion is involved here, it's legal to apply A & B to this function to get C | D. Conversely (A & B) -> (C | D) can also be seen as (A -> B) | (C -> D) (extensional equivalence). Since we restrict use of JavaScript's instanceof etc., these types become equal

Branching is special. If A <: distinguishable, then branch : A | B -> (A -> C) -> B | C is possible.
