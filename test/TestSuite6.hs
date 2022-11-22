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
contains :: (Show a, Eq a) => [a] -> [a] -> Bool
contains [] _ = True
contains (x:xs) y = if elem x y && contains xs y then True else error ("Item only in one of both lists: " ++ (show x))
 
-- Test data
la_fn_1 :: Lieferfenster -> Nat0
la_fn_1 (LF _ j) = if (j >= J2024) then 200 else 100
la_fn_2 (LF q _) = if (q >= Q3) then 200 else 100
la_fn_3 (LF _ _) = 200

ds_fn_1 :: Typ -> Datensatz 
ds_fn_1 _ = DS 1 1 (LA la_fn_1) Kein_Skonto
ds_fn_2 _ = DS 1 1 (LA la_fn_2) Kein_Skonto
ds_fn_3 _ = DS 1 1 (LA la_fn_3) Kein_Skonto
ds_fn_4 _ = Nicht_im_Sortiment
ds_fn_5 (M _) = DS 1 1 (LA la_fn_3) Kein_Skonto
ds_fn_5 _ = Nicht_im_Sortiment

ab_fn_1 :: Haendler -> Sortiment
ab_fn_1 _ = (Sort ds_fn_1)
ab_fn_2 h = if h > H5 then (Sort ds_fn_2) else (Sort ds_fn_1) 
ab_fn_3 h = if h == H1 then (Sort ds_fn_3) else (Sort ds_fn_4) 
ab_fn_4 h = if h == H1 then (Sort ds_fn_3) else (Sort ds_fn_4) 
ab_fn_5 _ = Sort ds_fn_5 

-- Tests
spec :: TestTree
spec = testGroup "Angabe6" [
    wgTests,
    wgfTests,
    sofortLieferfaehigTests
  ]
 
wgTests :: TestTree
wgTests =
  testGroup
    "Wertegraph Tests"
    [ testCase "Lieferausblick Wertegraph 1" $
      equals 
      (wg_la (LA la_fn_1)) 
       [ (LF {quartal = Q1, jahr = J2023}, 100), (LF {quartal = Q1, jahr = J2024}, 200), (LF {quartal = Q1, jahr = J2025}, 200),
          (LF {quartal = Q2, jahr = J2023}, 100), (LF {quartal = Q2, jahr = J2024}, 200), (LF {quartal = Q2, jahr = J2025}, 200),
          (LF {quartal = Q3, jahr = J2023}, 100), (LF {quartal = Q3, jahr = J2024}, 200), (LF {quartal = Q3, jahr = J2025}, 200),
          (LF {quartal = Q4, jahr = J2023}, 100), (LF {quartal = Q4, jahr = J2024}, 200), (LF {quartal = Q4, jahr = J2025}, 200) 
        ]
      @?= True 
    , testCase "Lieferausblick Wertegraph 2" $
      equals
      (wg_la (LA la_fn_2))
      [ 
        (LF {quartal = Q1, jahr = J2023}, 100), (LF {quartal = Q1, jahr = J2024}, 100), (LF {quartal = Q1, jahr = J2025}, 100),
        (LF {quartal = Q2, jahr = J2023}, 100), (LF {quartal = Q2, jahr = J2024}, 100), (LF {quartal = Q2, jahr = J2025}, 100),
        (LF {quartal = Q3, jahr = J2023}, 200), (LF {quartal = Q3, jahr = J2024}, 200), (LF {quartal = Q3, jahr = J2025}, 200),
        (LF {quartal = Q4, jahr = J2023}, 200), (LF {quartal = Q4, jahr = J2024}, 200), (LF {quartal = Q4, jahr = J2025}, 200) 
      ]
      @?= True
    , testCase "Sortiment Wertegraph 1" $
      equals
      (wg_so (Sort ds_fn_1))
      [ (M M2, ds_fn_1 (M M2)), (M M1, ds_fn_1 (M M1)), (M M3, ds_fn_1 (M M3)), (M M4, ds_fn_1 (M M4)), (M M5, ds_fn_1 (M M5)),
        (T T1, ds_fn_1 (T T1)), (T T2, ds_fn_1 (T T2)), (T T3, ds_fn_1 (T T3)), (T T4, ds_fn_1 (T T4)),
        (S S1, ds_fn_1 (S S1)), (S S2, ds_fn_1 (S S2)), (S S3, ds_fn_1 (S S3))
      ]
      @?= True
    , testCase "Sortiment Wertegraph 2" $
      equals
      (wg_so (Sort ds_fn_2))
      [ (S S1, ds_fn_2 (S S1)), (S S2, ds_fn_2 (S S2)), (S S3, ds_fn_2 (S S3)),
        (T T1, ds_fn_2 (T T1)), (T T2, ds_fn_2 (T T2)), (T T3, ds_fn_2 (T T3)), (T T4, ds_fn_2 (T T4)),
        (M M1, ds_fn_2 (M M1)), (M M2, ds_fn_2 (M M2)), (M M3, ds_fn_2 (M M3)), (M M4, ds_fn_2 (M M4)), (M M5, ds_fn_2 (M M5))
      ]
      @?= True
    , testCase "Anbieter Wertegraph 1" $
      equals
      (wg_ab (A ab_fn_1))
      [ (H1, ab_fn_1 H1), (H2, ab_fn_1 H2), (H3, ab_fn_1 H3), (H4, ab_fn_1 H4), (H5, ab_fn_1 H5),
        (H6, ab_fn_1 H6), (H7, ab_fn_1 H7), (H8, ab_fn_1 H8), (H9, ab_fn_1 H9), (H10, ab_fn_1 H10)
      ]
      @?= True
    , testCase "Anbieter Wertegraph 2" $
      equals
      (wg_ab (A ab_fn_2))
      [ (H1, ab_fn_2 H1), (H2, ab_fn_2 H2), (H3, ab_fn_2 H3), (H4, ab_fn_2 H4), (H5, ab_fn_2 H5),
        (H6, ab_fn_2 H6), (H7, ab_fn_2 H7), (H8, ab_fn_2 H8), (H9, ab_fn_2 H9), (H10, ab_fn_2 H10)
      ]
      @?= True
    ]

wgfTests :: TestTree
wgfTests =
  testGroup
    "Wgf Tests"
    [ testCase "Lieferausblick wohlgeformt Fehler 1" $
      expectSomeError "Ausblickfehler" $ wgf_fehler (LA la_fn_1)
    , testCase "Sortiment wohlgeformt Fehler 1" $
      expectSomeError "Sortimentfehler" $ wgf_fehler (Sort ds_fn_2)
    , testCase "Anbieter wohlgeformt Fehler 1" $
      expectSomeError "Anbieterfehler" $ wgf_fehler (A ab_fn_1)
    ]
    
sofortLieferfaehigTests :: TestTree
sofortLieferfaehigTests = 
  testGroup
    "SofortLieferfähig Tests"
    [ testCase "Sofort lieferbar 1" $
      sofort_lieferfaehig (M M1) (A ab_fn_3)
      @?= [H1]
    , testCase "Sofort lieferbar 2" $
      sofort_lieferfaehig (S S1) (A ab_fn_5)
      @?= []
    , testCase "Sofort lieferbar 3" $
      sofort_lieferfaehig (M M4) (A ab_fn_5)
      @?= [H10, H9 .. H1]
    ]