{-# LANGUAGE OverloadedStrings #-}
module LambdaPatternTests (spec) where

import Test.Hspec
import TestUtils (evalMainToJsValueString, expectRuntimeError)
import Data.Text (Text)

spec :: Spec
spec = describe "Lambda Expression Pattern Matching Tests" $ do

  -- 1. Variable Pattern
  it "evaluates lambda with variable pattern" $ do
    let code = "val main = (x -> x + 1) 10;"
    result <- evalMainToJsValueString code
    result `shouldBe` "11" -- Assuming numbers are output without .0 for integers

  -- 2. Tuple Pattern
  it "evaluates lambda with tuple pattern" $ do
    let code = "val main = ((a, b) -> a * b) (3, 4);"
    result <- evalMainToJsValueString code
    result `shouldBe` "12"

  -- 3. Constructor Patterns
  describe "Constructor Patterns" $ do
    let adtDefs = "data Opt = Some Number | Nn \n"
    
    it "evaluates lambda with constructor pattern (Some)" $ do
      let code = adtDefs <> "val main = ((Some x) -> x) (Some 5);"
      result <- evalMainToJsValueString code
      result `shouldBe` "5"

    it "evaluates lambda with constructor pattern (Nn)" $ do
      let code = adtDefs <> "val main = (Nn -> 0) Nn;"
      result <- evalMainToJsValueString code
      result `shouldBe` "0"

  -- 4. Wildcard in Tuple
  it "evaluates lambda with wildcard in tuple pattern" $ do
    let code = "val main = ((_, b) -> b + 1) (10, 20);"
    result <- evalMainToJsValueString code
    result `shouldBe` "21"

  -- 5. Nested Patterns
  -- PConstructor currently binds one variable. The JS deconstruction also assumes this.
  -- So, we test binding a tuple and then deconstructing it with `let`.
  describe "Nested Patterns (PConstructor binding a tuple)" $ do
    let adtDefs = "data PairWrapper = PW (Number, Number) \n" <>
                  "val getFirst = (p -> (let (a,_) = p in a)); \n" -- Helper to extract first element of a pair
    
    it "evaluates lambda with PConstructor binding a tuple, then deconstructed" $ do
      let code = adtDefs <> 
                 "val extractWrappedPairPayload = (PW x) -> x; \n" <> -- x is bound to (Number, Number)
                 "val main = getFirst (extractWrappedPairPayload (PW (10, 20)));"
      result <- evalMainToJsValueString code
      result `shouldBe` "10"

  -- 6. Type Annotated Lambda Pattern
  describe "Type Annotated Lambda Patterns" $ do
    it "evaluates lambda with type-annotated variable pattern" $ do
      let code = "val main = ((x: Number) -> x * 2) 7;"
      result <- evalMainToJsValueString code
      result `shouldBe` "14"

    it "evaluates lambda with type-annotated tuple pattern" $ do
      -- Note: Memento tuple type syntax is (Type, Type), not Type Tuple
      let code = "val main = (((a,b): (Number,Number)) -> a - b) (10, 3);"
      result <- evalMainToJsValueString code
      result `shouldBe` "7"

  -- 7. Runtime Pattern Match Failure
  describe "Runtime Pattern Match Failures" $ do
    let adtDefs = "data Opt = Some Number | Nn \n"

    it "fails for constructor mismatch" $ do
      let code = adtDefs <> "val main = ((Some x) -> x) Nn;"
      expectRuntimeError code "Lambda argument pattern mismatch"

    it "fails for tuple arity mismatch (too many elements)" $ do
      let code = "val main = ((a,b) -> a+b) (1,2,3);"
      expectRuntimeError code "Lambda argument pattern mismatch"
      
    it "fails for tuple arity mismatch (too few elements)" $ do
      let code = "val main = ((a,b,c) -> a+b+c) (1,2);"
      expectRuntimeError code "Lambda argument pattern mismatch"

    it "fails for basic type mismatch in pattern (e.g., number vs constructor)" $ do
      let code = adtDefs <> "val main = ((Some x) -> x) 10;"
      expectRuntimeError code "Lambda argument pattern mismatch"

    it "fails for PNumber pattern mismatch" $ do
      let code = "val main = ((10) -> 1) 11;"
      expectRuntimeError code "Lambda argument pattern mismatch"

    it "fails for PBool pattern mismatch" $ do
      let code = "val main = ((true) -> 1) false;"
      expectRuntimeError code "Lambda argument pattern mismatch"
