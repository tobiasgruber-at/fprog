> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollstaendigen Sie auch die vorgegebenen Kommentaranfaenge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen muessen durch mindestens eine Leerzeile getrennt sein!


Als erstes fuehren wir den Typalias Nat1 ein; seine Werte verwenden wir fuer
die Darstellung des Typs von Matrizen:

> type Nat1 = Int
> type Typ  = (Nat1,Nat1)

Matrizen modellieren wir als Listen von (Matrix-) Zeilen ueber entsprechenden
selbstdefierten Listentypen:

Zur Namenswahl: LE fuer `Letztes Element', E fuer `Element'

> data Zeile = LE Int                       
>              | E Int Zeile deriving Show

Zur Namenswahl: LZ fuer `Letzte Zeile, Z fuer `Zeile'

> data Matrix = LZ Zeile                       
>               | Z Zeile Matrix deriving Show  

Um mit Argumenten umzugehen, die keine Matrix darstellen oder im Typ nicht
zueinander passen, fuehren wir den Wert fehler als fehleranzeigenden Wert
ein (aufgrund unserer Festlegung von fehler bedeutet das, dass die Rueckgabe
dieses Werts gleichbedeutend mit dem Aufruf der Funktion error mit dem 
Argument "Argument(e) typfehlerhaft" ist und die Programmausfuehrung mit 
Ausgabe der Zeichenreihe "Argument(e) typfehlerhaft" endet).

> fehler = error "Argument(e) typfehlerhaft"

Abschliessend fuehren wir den algebraischen Datentyp Matrixtyp ein:

> data Matrixtyp = Matrix_vom_Typ Typ 
>                   | KeineMatrix deriving (Eq,Show)


Aufgabe A.1

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp (LZ z) = matrixtyp' (LZ z) (1) (elemente z)
> matrixtyp (Z z m) = matrixtyp' m (1 + zeilen m) (elemente z)

> matrixtyp' :: Matrix -> Nat1 -> Nat1 -> Matrixtyp
> matrixtyp' (LZ z) m n
>  | elemente z == n = Matrix_vom_Typ (m, n)
>  | otherwise = KeineMatrix
> matrixtyp' (Z z mat) m n
>  | elemente z == n = matrixtyp' mat m n
>  | otherwise = KeineMatrix

> zeilen :: Matrix -> Nat1
> zeilen (LZ _) = 1
> zeilen (Z _ m) = 1 + zeilen m

> elemente :: Zeile -> Nat1
> elemente (LE _) = 1
> elemente (E _ z) = 1 + elemente z

Knapp, aber gut nachvollziebar geht matrixtyp folgendermassen vor: 
...


Aufgabe A.2

> instance Eq Matrix where
>  (LZ z1) == (LZ z2) = z1 == z2
>  (Z z1 w1) == (Z z2 w2) = if valide' (Z z1 w1) (Z z2 w2) then (LZ z1) == (LZ z2) && w1 == w2 else fehler
>  m1 == m2 = if valide' m1 m2 then False else fehler

> instance Eq Zeile where
>  (LE w1) == (LE w2) = w1 == w2
>  (E w1 z1) == (E w2 z2) = (w1 == w2) && (z1 == z2)
>  z1 == z2 = False

> valide :: Matrix -> Bool
> valide m = matrixtyp m /= KeineMatrix

> valide' :: Matrix -> Matrix -> Bool
> valide' m1 m2 = valide m1 && valide m2

Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
...
 

Aufgabe A.3

> instance Num Matrix where
>  m1 + m2 = matrixOperation (+) m1 m2
>  m1 - m2 = matrixOperation (-) m1 m2
>  abs m = matrixOperation' (abs) m
>  fromInteger z = (LZ (LE (fromInteger z)))
>  m1 * m2 = error "(*) bleibt unimplementiert!"
>  negate m = error "negate bleibt unimplementiert!"
>  signum m = error "signum bleibt unimplementiert!"

> matrixOperation :: (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
> matrixOperation fn (LZ z1) (LZ z2) = (LZ (zeilenOperation fn z1 z2))
> matrixOperation fn (Z z1 m1) (Z z2 m2)
>  | gleicherTyp (Z z1 m1) (Z z2 m2) = (Z (zeilenOperation fn z1 z2) (matrixOperation fn m1 m2)) 
>  | otherwise = fehler
> matrixOperation _ m1 m2 = fehler

> matrixOperation' :: (Int -> Int) -> Matrix -> Matrix
> matrixOperation' fn (LZ z) = (LZ (zeilenOperation' fn z))
> matrixOperation' fn (Z z m) = if valide (Z z m) then (Z (zeilenOperation' fn z) (matrixOperation' fn m)) else fehler

> zeilenOperation :: (Int -> Int -> Int) -> Zeile -> Zeile -> Zeile
> zeilenOperation fn (LE i1) (LE i2) = (LE (fn i1 i2))
> zeilenOperation fn (E i1 z1) (E i2 z2) = (E (fn i1 i2) (zeilenOperation fn z1 z2))
> zeilenOperation _ _ _ = fehler

> zeilenOperation' :: (Int -> Int) -> Zeile -> Zeile
> zeilenOperation' fn (LE i) = (LE (fn i))
> zeilenOperation' fn (E i z) = (E (fn i) (zeilenOperation' fn z))

> gleicherTyp :: Matrix -> Matrix -> Bool
> gleicherTyp m1 m2 = (valide' m1 m2) && (matrixtyp m1) == (matrixtyp m2)

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor:
... 






