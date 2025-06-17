-- Demo module showing the type solver working with simple examples
module Language.Memento.TypeSolver.Demo where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Memento.TypeSolver.Types
import Language.Memento.TypeSolver.Solver

-- Demo functions to test the type solver

-- Example 1: Simple variable constraints
demo1 :: Either String (Map.Map T.Text Type)
demo1 = 
  let x = TypeVar (T.pack "x")
      y = TypeVar (T.pack "y")
      constraints = Set.fromList 
        [ Subtype (TVar x) TNumber
        , Subtype (TVar y) TBool
        ]
  in case solveConstraints constraints of
    Success subst -> Right $ Map.fromList 
      [ (T.pack "x", applySubst subst (TVar x))
      , (T.pack "y", applySubst subst (TVar y))
      ]
    Contradiction -> Left "Type contradiction"
    Ambiguous _ -> Left "Ambiguous types"

-- Example 2: Function type constraints  
demo2 :: Either String (Map.Map T.Text Type)
demo2 = 
  let f = TypeVar (T.pack "f")
      x = TypeVar (T.pack "x")
      constraints = Set.fromList
        [ Subtype (TVar f) (TFunction TNumber TBool)
        , Subtype TNumber (TVar x)
        ]
  in case solveConstraints constraints of
    Success subst -> Right $ Map.fromList
      [ (T.pack "f", applySubst subst (TVar f))
      , (T.pack "x", applySubst subst (TVar x))
      ]
    Contradiction -> Left "Type contradiction"
    Ambiguous _ -> Left "Ambiguous types"

-- Example 3: Union type constraints
demo3 :: Either String (Map.Map T.Text Type)
demo3 = 
  let x = TypeVar (T.pack "x")
      unionType = mkUnion [TNumber, TBool]
      constraints = Set.fromList
        [ Subtype (TVar x) unionType
        ]
  in case solveConstraints constraints of
    Success subst -> Right $ Map.fromList
      [ (T.pack "x", applySubst subst (TVar x))
      ]
    Contradiction -> Left "Type contradiction"
    Ambiguous _ -> Left "Ambiguous types"

-- Example 4: Contradiction example
demo4 :: Either String (Map.Map T.Text Type)
demo4 = 
  let x = TypeVar (T.pack "x")
      constraints = Set.fromList
        [ Subtype (TVar x) TNumber
        , Subtype (TVar x) TBool
        , Subtype TNumber TBool  -- This should cause contradiction
        ]
  in case solveConstraints constraints of
    Success subst -> Right $ Map.fromList
      [ (T.pack "x", applySubst subst (TVar x))
      ]
    Contradiction -> Left "Type contradiction (expected)"
    Ambiguous _ -> Left "Ambiguous types"

-- Run all demos
runAllDemos :: IO ()
runAllDemos = do
  putStrLn "=== Type Solver Demos ==="
  
  putStrLn "\nDemo 1: Simple variable constraints"
  case demo1 of
    Right env -> putStrLn $ "Success: " ++ show env
    Left err -> putStrLn $ "Error: " ++ err
  
  putStrLn "\nDemo 2: Function type constraints"
  case demo2 of
    Right env -> putStrLn $ "Success: " ++ show env
    Left err -> putStrLn $ "Error: " ++ err
  
  putStrLn "\nDemo 3: Union type constraints"
  case demo3 of
    Right env -> putStrLn $ "Success: " ++ show env
    Left err -> putStrLn $ "Error: " ++ err
  
  putStrLn "\nDemo 4: Contradiction example"
  case demo4 of
    Right env -> putStrLn $ "Success: " ++ show env
    Left err -> putStrLn $ "Error: " ++ err