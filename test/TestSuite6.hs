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
same_items x y = length x == length y && contains x y && contains y x 
contains :: (Show a, Eq a) => [a] -> [a] -> Bool
contains [] _ = True
contains (x:xs) y = if elem x y && contains xs y then True else error ("Item only in one of both lists: " ++ (show x))
 
-- Test data
la_fn_1 :: Lieferfenster -> Nat0
la_fn_1 (LF _ j) = if (j >= J2024) then 200 else 100
la_fn_2 :: Lieferfenster -> Nat0
la_fn_2 (LF q _) = if (q >= Q3) then 200 else 100
la_fn_3 :: Lieferfenster -> Nat0
la_fn_3 (LF _ _) = 200
la_fn_4 :: Lieferfenster -> Nat0
la_fn_4 (LF _ J2024) = 3
la_fn_4 (LF _ _) = 0
la_fn_5 :: Lieferfenster -> Nat0
la_fn_5 (LF Q2 _) = 3
la_fn_5 (LF _ _) = 0

ds_fn_1 :: Typ -> Datensatz 
ds_fn_1 _ = DS 1 1 (LA la_fn_1) Kein_Skonto
ds_fn_2 :: Typ -> Datensatz 
ds_fn_2 _ = DS 1 1 (LA la_fn_2) Kein_Skonto
ds_fn_3 :: Typ -> Datensatz 
ds_fn_3 (M _) = DS 1 1 (LA la_fn_3) Kein_Skonto
ds_fn_3 _ = Nicht_im_Sortiment

ab_fn_1 _ = (Sort ds_fn_1)
ab_fn_2 h = if h > H5 then (Sort ds_fn_2) else (Sort ds_fn_1) 
ab_fn_2 _ = (Sort ds_fn_1)
ab_fn_2 _ = Sort ds_fn_3 

-- Tests
spec :: TestTree
spec = testGroup "Angabe6" [
    wgTests,
    wgfTests,
    sofortLieferfaehigTests,
    sofortErhaeltlichTests,
    guenstigsteLieferantenTests,
    guenstigsteLieferantenImLFTests
  ]
 
wgTests :: TestTree
wgTests =
  testGroup
    "Wertegraph Tests"
    [ testCase "Lieferausblick Wertegraph 1" $
      same_items 
      (wg_la (LA la_fn_1)) 
       [ (LF {quartal = Q1, jahr = J2023}, 100), (LF {quartal = Q1, jahr = J2024}, 200), (LF {quartal = Q1, jahr = J2025}, 200),
          (LF {quartal = Q2, jahr = J2023}, 100), (LF {quartal = Q2, jahr = J2024}, 200), (LF {quartal = Q2, jahr = J2025}, 200),
          (LF {quartal = Q3, jahr = J2023}, 100), (LF {quartal = Q3, jahr = J2024}, 200), (LF {quartal = Q3, jahr = J2025}, 200),
          (LF {quartal = Q4, jahr = J2023}, 100), (LF {quartal = Q4, jahr = J2024}, 200), (LF {quartal = Q4, jahr = J2025}, 200) 
        ]
      @?= True 
    , testCase "Lieferausblick Wertegraph 2" $
      same_items
      (wg_la (LA la_fn_2))
      [ 
        (LF {quartal = Q1, jahr = J2023}, 100), (LF {quartal = Q1, jahr = J2024}, 100), (LF {quartal = Q1, jahr = J2025}, 100),
        (LF {quartal = Q2, jahr = J2023}, 100), (LF {quartal = Q2, jahr = J2024}, 100), (LF {quartal = Q2, jahr = J2025}, 100),
        (LF {quartal = Q3, jahr = J2023}, 200), (LF {quartal = Q3, jahr = J2024}, 200), (LF {quartal = Q3, jahr = J2025}, 200),
        (LF {quartal = Q4, jahr = J2023}, 200), (LF {quartal = Q4, jahr = J2024}, 200), (LF {quartal = Q4, jahr = J2025}, 200) 
      ]
      @?= True
    , testCase "Sortiment Wertegraph 1" $
      same_items
      (wg_so (Sort ds_fn_1))
      [ (M M2, ds_fn_1 (M M2)), (M M1, ds_fn_1 (M M1)), (M M3, ds_fn_1 (M M3)), (M M4, ds_fn_1 (M M4)), (M M5, ds_fn_1 (M M5)),
        (T T1, ds_fn_1 (T T1)), (T T2, ds_fn_1 (T T2)), (T T3, ds_fn_1 (T T3)), (T T4, ds_fn_1 (T T4)),
        (S S1, ds_fn_1 (S S1)), (S S2, ds_fn_1 (S S2)), (S S3, ds_fn_1 (S S3))
      ]
      @?= True
    , testCase "Sortiment Wertegraph 2" $
      same_items
      (wg_so (Sort ds_fn_2))
      [ (S S1, ds_fn_2 (S S1)), (S S2, ds_fn_2 (S S2)), (S S3, ds_fn_2 (S S3)),
        (T T1, ds_fn_2 (T T1)), (T T2, ds_fn_2 (T T2)), (T T3, ds_fn_2 (T T3)), (T T4, ds_fn_2 (T T4)),
        (M M1, ds_fn_2 (M M1)), (M M2, ds_fn_2 (M M2)), (M M3, ds_fn_2 (M M3)), (M M4, ds_fn_2 (M M4)), (M M5, ds_fn_2 (M M5))
      ]
      @?= True
    , testCase "Anbieter Wertegraph 1" $
      same_items
      (wg_ab (A ab_fn_1))
      [ (H1, ab_fn_1 H1), (H2, ab_fn_1 H2), (H3, ab_fn_1 H3), (H4, ab_fn_1 H4), (H5, ab_fn_1 H5),
        (H6, ab_fn_1 H6), (H7, ab_fn_1 H7), (H8, ab_fn_1 H8), (H9, ab_fn_1 H9), (H10, ab_fn_1 H10)
      ]
      @?= True
    , testCase "Anbieter Wertegraph 2" $
      same_items
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
      sofort_lieferfaehig (M M1) (A (\x ->
        if x == H1 then Sort (\_ -> DS 1 1 (LA (\_ -> 200)) Kein_Skonto) 
        else Sort (\_ -> Nicht_im_Sortiment)
      ))
      @?= [H1]
    , testCase "Sofort lieferbar 2" $
      sofort_lieferfaehig (S S1) (A (\x ->
        if x <= H5 then Sort (\_ -> DS 3 10 (LA (\_ -> 200)) Kein_Skonto) 
        else Sort (\_ -> Nicht_im_Sortiment)
      ))
      @?= [H5, H4 .. H1]
    , testCase "Sofort lieferbar 3" $
      sofort_lieferfaehig (M M4) (A ab_fn_2)
      @?= [H10, H9 .. H1]
    , testCase "Nicht lieferbar 1" $
      sofort_lieferfaehig (S S1) (A (\x -> (Sort (\_ -> Nicht_im_Sortiment))))
      @?= []
    , testCase "Nicht lieferbar 2" $
      sofort_lieferfaehig (S S1) (A (\x ->
        if x <= H5 then Sort (\_ -> DS 10 0 (LA (\_ -> 200)) Kein_Skonto) 
        else Sort (\_ -> Nicht_im_Sortiment)
      ))
      @?= []
    ]

sofortErhaeltlichTests :: TestTree
sofortErhaeltlichTests = 
  testGroup
    "sofortErhaeltlichTests Tests"
    [ testCase "Sofort erhältlich 1" $
      sofort_erhaeltliche_Stueckzahl (M M1) (A (\x ->
        if elem x [H3 .. H6] then Sort (\_ -> DS 3 10 (LA (\_ -> 200)) Kein_Skonto) 
        else Sort (\_ -> Nicht_im_Sortiment)
      ))
      @?= (40, 120)
    , testCase "Sofort erhältlich 2" $
      sofort_erhaeltliche_Stueckzahl (M M1) (A (\x ->
        if x <= H3 then (Sort (\_ -> DS 2 3 (LA (\_ -> 200)) Kein_Skonto)) 
        else Sort (\_ -> Nicht_im_Sortiment)
      ))
      @?= (9, 18)
    , testCase "Nicht erhaeltlich 1" $
      sofort_erhaeltliche_Stueckzahl (S S1) (A (\x -> (Sort (\_ -> Nicht_im_Sortiment))))
      @?= (0, 0)
    , testCase "Nicht erhaeltlich 1" $
      sofort_erhaeltliche_Stueckzahl (S S1) (A (\x ->
        if x <= H5 then Sort (\_ -> DS 10 0 (LA (\_ -> 200)) Kein_Skonto) 
        else Sort (\_ -> Nicht_im_Sortiment)
      ))
      @?= (0, 0)
    ]
    
guenstigsteLieferantenTests :: TestTree
guenstigsteLieferantenTests =
  testGroup
  "guenstigsteLieferantenTests Tests"
  [ testCase "Günstigste Lieferanten 1" $
    guenstigste_Lieferanten (M M1) (LF Q1 J2023) (A (\x ->
      if x == H1 then Sort (\_ -> DS 1 0 (LA (\_ -> 3)) Kein_Skonto) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= Just [H1]
  , testCase "Günstigste Lieferanten 2" $
    guenstigste_Lieferanten (M M1) (LF Q1 J2023) (A (\x ->
      if x <= H2 then Sort (\_ -> DS 1 0 (LA (\_ -> 3)) Kein_Skonto) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= Just [H2, H1]
  , testCase "Günstigste Lieferanten 3" $
    guenstigste_Lieferanten (M M1) (LF Q1 J2023) (A (\x ->
      if x == H2 then Sort (\_ -> DS 1 1 (LA (\_ -> 2)) Kein_Skonto) 
      else Sort (\_ -> DS 2 1 (LA (\_ -> 3)) Kein_Skonto)
    ))
    @?= Just [H2]
  , testCase "Günstigste Lieferanten 4" $
    guenstigste_Lieferanten (M M1) (LF Q1 J2023) (A (\x ->
      if x == H5 then Sort (\_ -> DS 1 0 (LA (\_ -> 3)) Kein_Skonto) 
      else if x == H2 then Sort (\_ -> DS 1 0 (LA (\_ -> 2)) Kein_Skonto) 
      else if x == H3 then Sort (\_ -> DS 2 0 (LA (\_ -> 3)) Kein_Skonto) 
      else if x == H7 then Sort (\_ -> DS 1 0 (LA (\_ -> 3)) Kein_Skonto) 
      else if x == H9 then Sort (\_ -> DS 1 0 (LA la_fn_4) Kein_Skonto) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= Just [H7, H5, H2]
  , testCase "Keine Lieferanten 1" $
    guenstigste_Lieferanten (M M1) (LF Q1 J2023) (A (\x -> Sort (\_ -> Nicht_im_Sortiment)))
    @?= Nothing
  , testCase "Keine Lieferanten 2" $
    guenstigste_Lieferanten (M M1) (LF Q1 J2023) (A (\x ->
      if x <= H3 then Sort (\_ -> DS 1 0 (LA (\_ -> 0)) Kein_Skonto) 
      else if x <= H7 then Sort (\_ -> DS 1 0 (LA la_fn_4) Kein_Skonto) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= Nothing
  ]
  
guenstigsteLieferantenImLFTests :: TestTree
guenstigsteLieferantenImLFTests =
  testGroup
  "guenstigsteLieferantenImLFTests Tests"
  [ testCase "Günstigste Lieferanten 1" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 J2023) 3 (A (\x ->
      if x == H1 then Sort (\_ -> DS 200 0 (LA (\_ -> 3)) DreiProzent) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= [(H1, (EUR 590))]
  , testCase "Günstigste Lieferanten 2" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 J2023) 3 (A (\x ->
      if x <= H2 then Sort (\_ -> DS 200 0 (LA (\_ -> 3)) DreiProzent) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= [(H2, (EUR 590)), (H1, (EUR 590))]
  , testCase "Günstigste Lieferanten 3" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 J2023) 2 (A (\x ->
      if x == H2 then Sort (\_ -> DS 150 0 (LA (\_ -> 4)) ZehnProzent) 
      else if x == H6 then Sort (\_ -> DS 100 0 (LA (\_ -> 1)) ZehnProzent) 
      else if x == H1 then Sort (\_ -> DS 200 0 (LA (\_ -> 2)) DreiProzent) 
      else if x == H4 then Sort (\_ -> DS 135 0 (LA (\_ -> 3)) Kein_Skonto) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= [(H4, (EUR 270)), (H2, (EUR 270))]
  , testCase "Günstigste Lieferanten 4" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 J2023) 4 (A (\x ->
      if x <= H2 then Sort (\_ -> DS 95 1 (LA (\_ -> 3)) DreiProzent) 
      else if x <= H4 then Sort (\_ -> DS 100 5 (LA (\_ -> 5)) DreiProzent) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= [(H4, (EUR 390)), (H3, (EUR 390))]
  , testCase "Günstigste Lieferanten 5" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 J2023) 2 (A (\x ->
      if x <= H2 then Sort (\_ -> DS 95 1 (LA (\_ -> 3)) Kein_Skonto) 
      else if x <= H4 then Sort (\_ -> DS 100 1 (LA (\_ -> 5)) DreiProzent) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= [(H2, EUR 190), (H1, EUR 190)]
  , testCase "Günstigste Lieferanten 6" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 J2023) 2 (A (\x ->
      if x == H2 then Sort (\_ -> DS 100 1 (LA (\_ -> 5)) Kein_Skonto)
      else if x == H4 then Sort (\_ -> DS 100 1 (LA (\_ -> 5)) DreiProzent)
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= [(H4, EUR 200), (H2, EUR 200)]
  , testCase "Keine Lieferanten 1" $
    guenstigste_Lieferanten_im_Lieferfenster (T T2) (LF Q1 J2023) 2 (A (\x ->
      if x == H1 then Sort (\_ -> DS 100 1 (LA la_fn_4) Kein_Skonto) 
      else if x == H2 then Sort ds_fn_3 
      else if x == H3 then Sort (\_ -> DS 100 1 (LA (\_ -> 0)) Kein_Skonto) 
      else if x == H4 then Sort (\_ -> DS 100 1 (LA la_fn_5) Kein_Skonto) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= []
  , testCase "Keine Lieferanten 1" $
    guenstigste_Lieferanten_im_Lieferfenster (T T2) (LF Q1 J2023) 0 (A (\x ->
      if x == H2 then Sort (\_ -> DS 95 1 (LA (\_ -> 3)) Kein_Skonto) 
      else if x == H4 then Sort (\_ -> DS 100 1 (LA (\_ -> 5)) DreiProzent) 
      else Sort (\_ -> Nicht_im_Sortiment)
    ))
    @?= []
  ]