module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

-- Fuer A.1 bis A.4

data Lotterielos = Treffer 
                   | Niete 
                   | Freilos deriving (Eq,Show)
 
data Liste = Schluss Lotterielos
             | Kopf Lotterielos Liste deriving (Eq,Show)

data Baum = Blatt Lotterielos
            | Gabel Baum Lotterielos Baum deriving (Eq,Show)

data Liste' = Schluss' Baum
              | Kopf' Baum Liste' deriving (Eq,Show)

data Baum' = Blatt' Liste
             | Gabel' Baum' Liste Baum' deriving (Eq,Show)

type Loswert    = Lotterielos
type Auswertung = Ordering

type Differenz = Int

-- Aufgabe A.1

analysiere :: Liste -> Loswert -> Loswert -> Auswertung
analysiere l lw lw'
 | res < 0 = LT
 | res == 0 = EQ
 | otherwise = GT
 where
    res = differenz l lw lw' :: Differenz
    
differenz :: Liste -> Loswert -> Loswert -> Differenz
differenz (Schluss e) lw lw' = 
 | e == lw = 1 
 | e == lw' = -1
 | otherwise = 0  
differenz (Kopf e l) lw lw' = differenz (Schluss e) lw lw' + differenz l lw lw'
    
{- Knapp, aber gut nachvollziehbar geht analysiere folgendermassen vor:
    Die Hilfsfunktion differenz() bestimmt rekursiv die Differenz von der Anzahl an Vorkommnissen beider 
    gegebenen Loswerte in einer Liste. Der Basisfall (Schluss) gibt 1, 0 oder -1 zurück, je nachdem welchen Loswert der
    Schluss trägt. Der Kopf summiert rekursiv die Restliste mit dem eigenen Wert (welcher rekursiv als Schluss evaluiert
    wird).
    Anhand dieser Differenz evaluert analysiere() ob der erste Loswert öfter oder gleich / weniger oft in einer Liste 
    vorkommt.
-}



-- Aufgabe A.2

analysiere' :: Baum -> Loswert -> Loswert -> Auswertung
analysiere' b lw lw'
 | res < 0 = LT
 | res == 0 = EQ
 | otherwise = GT
 where
    res = differenz' b lw lw' :: Differenz

differenz' :: Baum -> Loswert -> Loswert -> Differenz
differenz' (Blatt e) lw lw' = 
 | e == lw = 1 
 | e == lw' = -1
 | otherwise = 0  
differenz' (Gabel b1 e b2) lw lw' = differenz' b1 lw lw' + differenz' (Blatt e) lw lw' + differenz' b2 lw lw'

{- Knapp, aber gut nachvollziehbar geht analysiere' folgendermassen vor:
    Die Hilfsfunktion differenz'() bestimmt rekursiv die Differenz von der Anzahl an Vorkommnissen beider 
    gegebenen Loswerte in einem Baum. Der Basisfall (Blatt) gibt 1, 0 oder -1 zurück, je nachdem welchen Loswert das
    Blatt trägt. Die Gabel summiert rekursiv die Teilbäume, sowie den eigenen Wert.
    Anhand dieser Differenz evaluert analysiere'() ob der erste Loswertöfter oder gleich / weniger oft in dem Baum 
    vorkommt.
-}



-- Aufgabe A.3

analysiere'' :: Liste' -> Loswert -> Loswert -> Auswertung
analysiere'' l lw lw'
 | res < 0 = LT
 | res == 0 = EQ
 | otherwise = GT
 where
    res = differenz'' l lw lw' :: Differenz

differenz'' :: Liste' -> Loswert -> Loswert -> Differenz
differenz'' (Schluss' t) lw lw' = differenz' t lw lw'  
differenz'' (Kopf' t l) lw lw' = differenz'' (Schluss' t) lw lw' + differenz'' l lw lw'

{- Knapp, aber gut nachvollziehbar geht analysiere'' folgendermassen vor:
    Die Hilfsfunktion differenz''() bestimmt rekursiv die Differenz von der Anzahl an Vorkommnissen beider 
    gegebenen Loswerte in einer Liste'. Der Basisfall (Schluss') evaluiert mittels differenz'() die Differenz vom Baum. 
    Der Kopf' summiert rekursiv die Restliste mit dem eigenen Wert, also dem Baum.
    Anhand dieser Differenz evaluert analysiere''() ob der erste Loswert öfter oder gleich / weniger oft in einer Liste 
    vorkommt.
-}



-- Aufgabe A.4

analysiere''' :: Baum' -> Loswert -> Loswert -> Auswertung
analysiere''' b lw lw'
 | res < 0 = LT
 | res == 0 = EQ
 | otherwise = GT
 where
    res = differenz''' b lw lw' :: Differenz

differenz''' :: Baum' -> Loswert -> Loswert -> Differenz
differenz''' (Blatt' l) lw lw' = differenz l lw lw'
differenz''' (Gabel' b1 l b2) lw lw' = differenz''' b1 lw lw' + differenz''' (Blatt' l) lw lw' + differenz''' b2 lw lw'

{- Knapp, aber gut nachvollziehbar geht analysiere''' folgendermassen vor:
    Die Hilfsfunktion differenz'''() bestimmt rekursiv die Differenz von der Anzahl an Vorkommnissen beider 
    gegebenen Loswerte in einem Baum'. Der Basisfall (Blatt') evaluiert mittels differenz() die Differenz der Liste. 
    Die Gabel' summiert rekursiv die Teilbäume, sowie den eigenen Wert, also der Liste.
    Anhand dieser Differenz evaluert analysiere'''() ob der erste Loswert öfter / weniger oft in dem Baum vorkommt.
-}
