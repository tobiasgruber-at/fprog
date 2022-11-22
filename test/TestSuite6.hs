module TestSuite6 where

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Control.Exception as Exc

import           Angabe6

-- check if error thrown
expectSomeError :: String -> a -> Assertion
expectSomeError msg val = do
  res <- Exc.try (Exc.evaluate val)
  case res of
    Left (Exc.ErrorCall m) -> if m == msg then pure () else assertFailure ("Expected error '" ++ msg ++ "' but got error '" ++ m ++ "'") 
    Right _ -> assertFailure "Expected error call but got none"


-- Check if two lists contain same items
equals x y = length x == length y && contains x y && contains y x 
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] y = True
contains (x:xs) y = elem x y && contains xs y
 
-- Test data
f_l :: [Lieferfenster]
f_l = [(LF q j) | q <- [Q1 .. Q4], j <- [J2023 .. J2025]]
t_l :: [Typ]
t_l = [M w | w <- [M1 .. M5]] ++ [T t | t <- [T1 .. T4]] ++ [S s | s <- [S1 .. S3]] 
h_l :: [Haendler]
h_l = [H1 .. H10]

la_fn_1 :: Lieferfenster -> Nat0
la_fn_1 (LF q j) = if (j >= J2024) then 200 else 100
la_fn_2 :: Lieferfenster -> Nat0
la_fn_2 (LF q j) = if (q >= Q3) then 200 else 100

ds_fn_1 :: Typ -> Datensatz 
ds_fn_1 _ = (DS 1 1 (LA la_fn_1) Kein_Skonto)
ds_fn_2 :: Typ -> Datensatz 
ds_fn_2 _ = (DS 1 1 (LA la_fn_2) Kein_Skonto)

ab_fn_1 :: Haendler -> Sortiment
ab_fn_1 _ = (Sort ds_fn_1)
ab_fn_2 :: Haendler -> Sortiment
ab_fn_2 h = if h > H5 then (Sort ds_fn_2) else (Sort ds_fn_1) 

-- Tests
spec :: TestTree
spec = testGroup "Angabe6" [
    wgTests
  ]
 
wgTests :: TestTree
wgTests =
  testGroup
    "Wertegraph Tests"
    [ testCase "Lieferausblick Wertegraph 1" $
      equals 
      (wg_la (LA la_fn_1)) 
      [(x, la_fn_1 x) | x <- f_l]
      @?= True 
    , testCase "Lieferausblick Wertegraph 2" $
      equals
      (wg_la (LA la_fn_2))
      [(x, la_fn_2 x) | x <- f_l]
      @?= True
    , testCase "Sortiment Wertegraph 1" $
      equals
      (wg_so (Sort ds_fn_1))
      [(x, ds_fn_1 x) | x <- t_l]
      @?= True
    , testCase "Sortiment Wertegraph 2" $
      equals
      (wg_so (Sort ds_fn_2))
      [(x, ds_fn_2 x) | x <- t_l]
      @?= True
    , testCase "Anbieter Wertegraph 1" $
      equals
      (wg_ab (A ab_fn_1))
      [(x, ab_fn_1 x) | x <- h_l]
      @?= True
    , testCase "Anbieter Wertegraph 2" $
      equals
      (wg_ab (A ab_fn_2))
      [(x, ab_fn_2 x) | x <- h_l]
      @?= True
    ]
