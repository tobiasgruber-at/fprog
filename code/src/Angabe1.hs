module Angabe1 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

-- Fuer A.1

type Nat0         = Int
type Zeichen      = Char
type Zeichenreihe = [Zeichen]
type Haeufigkeit  = Nat0
type Histogramm   = [(Zeichen,Haeufigkeit)]

-- Fuer A.2

type Gewicht        = Nat0
type Gewichtsverzeichnis = [(Zeichen,Gewicht)]
fehlerwert = -1


-- Aufgabe A.1

haeufigkeit :: Zeichenreihe -> Histogramm

{- Knapp, aber gut nachvollziehbar geht haufigkeit folgendermassen vor:
   ...
-}



-- Aufgabe A.2

gewicht :: Zeichenreihe -> Gewichtsverzeichnis -> Gewicht

{- Knapp, aber gut nachvollziehbar geht gewicht folgendermassen vor:
   ...
-}



-- Aufgabe A.3

korrigiere :: Gewichtsverzeichnis -> Gewichtsverzeichnis

{- Knapp, aber gut nachvollziehbar geht korrigiere folgendermassen vor:
   ...
-}



-- Aufgabe A.4

korrigiere' :: Gewichtsverzeichnis -> Gewichtsverzeichnis

{- Knapp, aber gut nachvollziehbar geht korrigiere' folgendermassen vor:
   ...
-}
