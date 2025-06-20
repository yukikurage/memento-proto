# Decompose

type constructor : `Maybe<1>, Maybe<T>` ...
literal constructor : `Some(1), None(), Some(number), Some(T)` ...

DC : DECOMPOSE or CONTR

| <:         |   PRIM    |   never   | unknown |    lit    |   var y   | type cnstr | lit cnstr |   func    |   union   |   inter   |  generic  |
| ---------- | :-------: | :-------: | :-----: | :-------: | :-------: | :--------: | :-------: | :-------: | :-------: | :-------: | :-------: |
| PRIM       |  STATIC   |  STATIC   | STATIC  |  STATIC   |   BOUND   |   CONTR    |   CONTR   |   CONTR   |  BRANCH   | DECOMPOSE |   CONTR   |
| never      |  STATIC   |  STATIC   | STATIC  |  STATIC   |    SAT    |    SAT     |    SAT    |    SAT    |    SAT    |    SAT    |    SAT    |
| unknown    |  STATIC   |  STATIC   | STATIC  |  STATIC   |   BOUND   |   CONTR    |   CONTR   |   CONTR   |  BRANCH   | DECOMPOSE |   CONTR   |
| lit        |  STATIC   |  STATIC   | STATIC  |  STATIC   |   BOUND   |   CONTR    |   CONTR   |   CONTR   |  BRANCH   | DECOMPOSE |   CONTR   |
| var x      |   BOUND   |   BOUND   |   SAT   |   BOUND   |   BOUND   |   BRANCH   |  BRANCH   |  BRANCH   |  BRANCH   | DECOMPOSE |   BOUND   |
| type cnstr |   CONTR   |   CONTR   |   SAT   |   CONTR   |  BRANCH   |     DC     |    DC     |    DC     |  BRANCH   | DECOMPOSE |   CONTR   |
| lit cnstr  |   CONTR   |   CONTR   |   SAT   |   CONTR   |  BRANCH   |     DC     |    DC     |    DC     |  BRANCH   | DECOMPOSE |   CONTR   |
| func       |   CONTR   |   CONTR   |   SAT   |   CONTR   |  BRANCH   |     DC     |    DC     |    DC     |  BRANCH   | DECOMPOSE |   CONTR   |
| union      | DECOMPOSE | DECOMPOSE |   SAT   | DECOMPOSE | DECOMPOSE | DECOMPOSE  | DECOMPOSE | DECOMPOSE | DECOMPOSE | DECOMPOSE | DECOMPOSE |
| inter      |  BRANCH   |  BRANCH   |   SAT   |  BRANCH   |  BRANCH   |   BRANCH   |  BRANCH   |  BRANCH   |  BRANCH   | DECOMPOSE |  BRANCH   |
| generic    |   CONTR   |   CONTR   |   SAT   |   CONTR   |   BOUND   |   CONTR    |   CONTR   |   CONTR   |  BRANCH   | DECOMPOSE |   CONTR   |

- LEFT RIGHT Concrete => STATIC
- LEFT RIGHT Same => SAT

- LEFT never => SAT
- RIGHT unknown => SAT
- LEFT union => DECOMPOSE
- RIGHT inter => DECOMPOSE
- LEFT inter => BRANCH
- RIGHT union => BRANCH
- Left var x, RIGHT (type cnstr, lit cnstr, func) => BRANCH
- LEFT (type constr, lit constr, func), RIGHT var y => BRANCH
- LEFT var x => BOUND
- RIGHT var y => BOUND
- LEFT (type cnstr, lit constr, func), RIGHT (type cnstr, lit cnstr, func) => DECOMPOSE or BRANCH
- otherwise => CONTR
