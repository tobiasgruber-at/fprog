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
haeufigkeit "" = [] 
haeufigkeit (x:xs) = (x, 1 + length [a | a <- xs, a == x]) : haeufigkeit [a | a <- xs, a /= x]

{- Knapp, aber gut nachvollziehbar geht haufigkeit folgendermassen vor:
    Zählt rekursiv für den ersten Charakter der übrigen ZK, wie oft dieser vorkommt. Das Ergebnis wird in einem Tupel
    an jenes Histogram geknüpft, welches die Auswertung der restlichen ZK (ohne dem ersten Zeichen) beinhaltet.
-}



-- Aufgabe A.2

gewicht :: Zeichenreihe -> Gewichtsverzeichnis -> Gewicht
gewicht "" v = 0
gewicht (x:xs) v 
 | length matches == 0 = gewicht'
 | length matches > 1 || gewicht' == -1 = -1
 | otherwise = snd (head matches) + gewicht'
 where 
    matches = [a | a <- v, fst a == x]
    gewicht' = gewicht xs v
    
{- Knapp, aber gut nachvollziehbar geht gewicht folgendermassen vor:
    Addiert rekursiv die Gewichtung des ersten Zeichen einer ZK zu der Gewichtung der restlichen ZK. Hierfür wird das
    Gewicht des entsprechenden Tupels des Gewichtungsverzeichnisses ausgeleesen. Im Falle eines unzulässigen 
    Verzeichnisses wird der Fehlerwert -1 in der Rekursion hochpropagiert.
-}



-- Aufgabe A.3

korrigiere :: Gewichtsverzeichnis -> Gewichtsverzeichnis
korrigiere [] = []
korrigiere (x:xs) = x : korrigiere [a | a <- xs, fst a /= fst x]

{- Knapp, aber gut nachvollziehbar geht korrigiere folgendermassen vor:
    Verkettet rekursiv den ersten Eintrag des Verzeichnisses mit den restlichen korrigierten Einträgen, welche nicht 
    das erste Zeichen enthalten dürfen.
-}



-- Aufgabe A.4

korrigiere' :: Gewichtsverzeichnis -> Gewichtsverzeichnis
korrigiere' [] = []
korrigiere' (x:xs) = (fst x, count) : korrigiere' [a | a <- xs, fst a /= fst x]
 where 
    matches = [a | a <- x:xs, fst a == fst x]
    count = sum [snd a | a <- matches]
    
{- Knapp, aber gut nachvollziehbar geht korrigiere' folgendermassen vor:
    Summiert die Gewichtung aller Einträge des Verzeichnisses, welche das Zeichen des ersten Eintrags enthalten. Dieser
    Eintrag wird anschließend rekursiv mit den restlichen, korrigierten Einträgen verkettet.
-}