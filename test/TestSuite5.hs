module TestSuite5 where

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Control.Exception as Exc

import           Angabe5

expectSomeError :: String -> a -> Assertion
expectSomeError msg val = do
  res <- Exc.try (Exc.evaluate val)
  case res of
    Left (Exc.ErrorCall m) -> if m == msg then pure () else assertFailure ("Expected error '" ++ msg ++ "' but got error '" ++ m ++ "'") 
    Right _ -> assertFailure "Expected error call but got none"

spec :: TestTree
spec = testGroup "Angabe5" [lieferausblickTests, sortimentTests, anbieterTests, sofortLieferfaehigTests, sofortErhaeltlichTests, guenstigsteLieferantenTests]

lieferausblickTests :: TestTree
lieferausblickTests =
  testGroup
    "Lieferausblick Tests"
    [ testCase "Lieferausblick ist wohlgeformt 1" $
      ist_wgf
        (LA
           [ ((LF Q1 2023), 2)
           , ((LF Q2 2023), 2)
           , ((LF Q1 2023), 2)
           , ((LF Q3 2025), 5)
           ]) @?=
      True
    , testCase "Lieferausblick ist wohlgeformt 2" $
      ist_wgf (LA [((LF Q1 2023), 0)]) @?= True
    , testCase "Lieferausblick ist wohlgeformt 3" $
      ist_wgf (LA [((LF Q1 2023), 1), ((LF Q1 2023), 0)]) @?= False
    , testCase "Lieferausblick ist nicht wohlgeformt 1" $
      ist_nwgf (LA [((LF Q1 2023), 0)]) @?= False
    , testCase "Lieferausblick wohlgeformt Fehler 1" $
      expectSomeError "Ausblickfehler" $ wgf_fehler (LA [((LF Q1 2023), 0)])
    ]

sortimentTests :: TestTree
sortimentTests =
  testGroup
    "Sortiment Tests"
    [ testCase "Sortiment ist wohlgeformt 1" $
      ist_wgf (Sort [((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))]) @?=
      True
    , testCase "Sortiment ist wohlgeformt 2" $
      ist_wgf
        (Sort
           [ ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
           , ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
           ]) @?=
      False
    , testCase "Sortiment ist nicht wohlgeformt 1" $
      ist_nwgf (Sort [((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))]) @?=
      False
    , testCase "Sortiment wohlgeformt Fehler 1" $
      expectSomeError "Sortimentfehler" $ wgf_fehler (Sort [((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))])
    ]

anbieterTests :: TestTree
anbieterTests =
  testGroup
    "Anbieter Tests"
    [ testCase "Anbieter ist wohlgeformt 1" $
      ist_wgf (A [(H1, (Sort []))]) @?= True
    , testCase "Anbieter ist wohlgeformt 2" $
      ist_wgf (A [(H1, (Sort [])), (H1, (Sort []))]) @?= False
    , testCase "Anbieter ist wohlgeformt 3" $
      ist_wgf
        (A [ (H1, (Sort []))
           , ( H2
             , (Sort [((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))]))
           ]) @?=
      True
    , testCase "Anbieter ist wohlgeformt 4" $
      ist_wgf
        (A [ ( H1
             , (Sort
                  [ ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
                  , ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
                  ]))
           ]) @?=
      False
    , testCase "Anbieter ist nicht wohlgeformt 1" $
      ist_nwgf (A [(H1, (Sort []))]) @?= False
    , testCase "Anbieter wohlgeformt Fehler 1" $
      expectSomeError "Anbieterfehler" $ wgf_fehler (A [(H1, (Sort []))])
    ]
    
sofortLieferfaehigTests :: TestTree
sofortLieferfaehigTests = 
  testGroup
    "SofortLieferfähig Tests"
    [ testCase "Sofort lieferbar 1" $
      sofort_lieferfaehig (M M1) (A [(H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))])
      @?= [H1]
    , testCase "Sofort lieferbar 2" $
      sofort_lieferfaehig (S S3) (A [(H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((T T2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))])
      @?= []
    , testCase "Sofort lieferbar 2" $
      sofort_lieferfaehig (T T2) (A [
        (H1, (Sort [((M M2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), ((T T2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))])),
        (H2, (Sort [((T T2), (DS 2 3 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), ((S S2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))]))
      ])
      @?= [H1, H2]
    , testCase "Sofort lieferbar fehlerhaft 1" $
      expectSomeError "Anbieterfehler" $ sofort_lieferfaehig (M M1) (A [(H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))])
    ]
    
sofortErhaeltlichTests :: TestTree
sofortErhaeltlichTests = 
  testGroup
    "sofortErhaeltlichTests Tests"
    [ testCase "Sofort erhältlich 1" $
      sofort_erhaeltliche_Stueckzahl (M M1) (A [(H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))])
      @?= (1, 1)
    , testCase "Sofort erhältlich 2" $
      sofort_erhaeltliche_Stueckzahl (M M3) (A [(H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))])
      @?= (0, 0)
    , testCase "Sofort erhältlich 3" $
      sofort_erhaeltliche_Stueckzahl (T T2) (A [
        (H1, (Sort [
          ((M M2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
          ((T T2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
        ])),
        (H2, (Sort [
          ((T T2), (DS 2 3 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
          ((S S2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
        ]))
      ])
      @?= (4, 7)
    , testCase "Sofort erhältlich fehlerhaft 1" $
      expectSomeError "Anbieterargumentfehler" $ sofort_erhaeltliche_Stueckzahl (M M1) (A [(H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))])
    ]

guenstigsteLieferantenTests :: TestTree
guenstigsteLieferantenTests =
  testGroup
  "guenstigsteLieferantenTests Tests"
  [ testCase "Günstigste Lieferanten 1" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H1, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))]))
    ])
    @?= Just [H1]
  , testCase "Günstigste Lieferanten 2" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H1, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])), 
      (H2, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 2)]) Kein_Skonto))]))
    ])
    @?= Just [H1, H2]
  , testCase "Günstigste Lieferanten 3" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H1, (Sort [(M M1, (DS 2 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])), 
      (H2, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 2)]) Kein_Skonto))]))
    ])
    @?= Just [H2]
  , testCase "Keine Lieferanten 1" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [])
    @?= Nothing  
  , testCase "Keine Lieferanten 2" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H1, (Sort [(M M1, (DS 2 0 (LA [((LF Q1 2023), 0)]) Kein_Skonto))])), 
      (H2, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2024), 3)]) Kein_Skonto))]))
    ])
    @?= Nothing
  , testCase "Günstigste Lieferanten fehlerhaft 1" $
    expectSomeError "Anbieterargumentfehler" $ guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))
    ])
  ]