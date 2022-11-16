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
spec = testGroup "Angabe5" [
    lieferausblickTests, 
    sortimentTests, 
    anbieterTests, 
    sofortLieferfaehigTests, 
    sofortErhaeltlichTests, 
    guenstigsteLieferantenImLFTests
  ]

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
        (H3, (Sort [((T T2), (DS 3 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), ((M M4), (DS 3 1 (LA [((LF Q2 2023), 0)]) Kein_Skonto))])),
        (H1, (Sort [((M M2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), ((T T2), (DS 2 0 (LA [((LF Q1 2023), 0)]) Kein_Skonto))])),
        (H2, (Sort [((S S1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), ((T T2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))])),
        (H4, (Sort [((T T2), (DS 2 3 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), ((S S2), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))]))
      ])
      @?= [H4, H3, H2]
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
    @?= Just [H2, H1]
  , testCase "Günstigste Lieferanten 3" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H1, (Sort [(M M1, (DS 2 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])), 
      (H2, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 2)]) Kein_Skonto))]))
    ])
    @?= Just [H2]
  , testCase "Günstigste Lieferanten 4" $
    guenstigste_Lieferanten (M M1) (LF Q1 2023) (A [
      (H5, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])), 
      (H2, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 2)]) Kein_Skonto))])),
      (H3, (Sort [(M M1, (DS 2 0 (LA [((LF Q1 2023), 5)]) Kein_Skonto))])),
      (H7, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])),
      (H9, (Sort [(M M1, (DS 1 0 (LA [((LF Q2 2023), 1)]) Kein_Skonto))])),
      (H4, (Sort [(M M1, (DS 1 0 (LA [((LF Q1 2023), 0)]) Kein_Skonto))]))
    ])
    @?= Just [H7, H5, H2]
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
 
guenstigsteLieferantenImLFTests :: TestTree
guenstigsteLieferantenImLFTests =
  testGroup
  "guenstigsteLieferantenImLFTests Tests"
  [ testCase "Günstigste Lieferanten 1" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 3 (A [
      (H1, (Sort [(M M1, (DS 200 0 (LA [((LF Q1 2023), 3)]) DreiProzent))]))
    ])
    @?= [(H1, (EUR 590))]
  , testCase "Günstigste Lieferanten 2" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 3 (A [
      (H2, (Sort [(M M1, (DS 200 0 (LA [((LF Q1 2023), 3)]) DreiProzent))])),
      (H1, (Sort [(M M1, (DS 200 0 (LA [((LF Q1 2023), 3)]) DreiProzent))]))
    ])
    @?= [(H2, (EUR 590)), (H1, (EUR 590))]
  , testCase "Günstigste Lieferanten 3" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H2, (Sort [(M M1, (DS 150 0 (LA [((LF Q1 2023), 4)]) ZehnProzent))])),
      (H6, (Sort [(M M1, (DS 100 0 (LA [((LF Q1 2023), 1)]) ZehnProzent))])),
      (H1, (Sort [(M M1, (DS 200 0 (LA [((LF Q1 2023), 2)]) DreiProzent))])),
      (H4, (Sort [(M M1, (DS 135 0 (LA [((LF Q1 2023), 3)]) Kein_Skonto))]))
    ])
    @?= [(H4, (EUR 270)), (H2, (EUR 270))]
  , testCase "Günstigste Lieferanten 4" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 4 (A [
      (H1, (Sort [(M M1, (DS 95 1 (LA [((LF Q1 2023), 3)]) DreiProzent))])),
      (H2, (Sort [(M M1, (DS 95 1 (LA [((LF Q1 2023), 3)]) DreiProzent))])),
      (H3, (Sort [(M M1, (DS 100 5 (LA [((LF Q1 2023), 5)]) DreiProzent))])),
      (H4, (Sort [(M M1, (DS 100 5 (LA [((LF Q1 2023), 5)]) DreiProzent))]))
    ])
    @?= [(H4, (EUR 390)), (H3, (EUR 390))]
  , testCase "Günstigste Lieferanten 5" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H1, (Sort [(M M1, (DS 95 1 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])),
      (H2, (Sort [(M M1, (DS 95 1 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])),
      (H3, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) DreiProzent))])),
      (H4, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) DreiProzent))]))
    ])
    @?= [(H2, EUR 190), (H1, EUR 190)]
  , testCase "Günstigste Lieferanten 6" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H2, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) Kein_Skonto))])),
      (H4, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) DreiProzent))]))
    ])
    @?= [(H4, EUR 200), (H2, EUR 200)]
  , testCase "Günstigste Lieferanten 7" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H2, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) Kein_Skonto))])),
      (H4, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) FuenfProzent))]))
    ])
    @?= [(H4, EUR 190)]
  , testCase "Keine Lieferanten 1" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H1, (Sort [(M M1, (DS 95 1 (LA [((LF Q2 2023), 3)]) Kein_Skonto))])),
      (H2, (Sort [(T T2, (DS 95 1 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])),
      (H3, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2024), 5)]) DreiProzent))])),
      (H4, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 0)]) DreiProzent))]))
    ])
    @?= []
  , testCase "Kein Lieferanten 2" $
    guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 0 (A [
      (H2, (Sort [(M M1, (DS 95 1 (LA [((LF Q1 2023), 3)]) Kein_Skonto))])),
      (H4, (Sort [(M M1, (DS 100 1 (LA [((LF Q1 2023), 5)]) DreiProzent))]))
    ])
    @?= []
    , testCase "Günstigste Lieferanten fehlerhaft 1" $
    expectSomeError "Anbieterargumentfehler" $ guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H1, (Sort [
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto)), 
        ((M M1), (DS 1 1 (LA [((LF Q1 2023), 0)]) Kein_Skonto))
      ]))
    ])
    , testCase "Günstigste Lieferanten im LF fehlerhaft 2" $
    expectSomeError "Anbieterargumentfehler" $ guenstigste_Lieferanten_im_Lieferfenster (M M1) (LF Q1 2023) 2 (A [
      (H1, (Sort [((M M1), (DS 1 1 (LA [
        ((LF Q1 2023), 0),
        ((LF Q1 2023), 1)
      ]) Kein_Skonto))]))
    ])
  ]