module Angabe7 where


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 }

data Skonto  = Kein_Skonto 
               | DreiProzent  
               | FuenfProzent 
               | ZehnProzent

data Waschmaschine    = M1 | M2 | M3 | M4 | M5
data Waeschetrockner  = T1 | T2 | T3 | T4
data Waescheschleuder = S1 | S2 | S3

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        }

newtype Lieferausblick  = LA (Lieferfenster -> Nat0)
newtype Lieferausblick' = LA' [(Lieferfenster,Nat0)]

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment

data Datensatz' 
   = DS' { preis_in_euro' :: Nat1,
           sofort_lieferbare_stueckzahl' :: Nat0,
           lieferbare_stueckzahl_im_Zeitfenster' :: Lieferausblick',
           skonto' :: Skonto
        }
     | Nicht_im_Sortiment'

newtype Sortiment  = Sort (Typ -> Datensatz)
newtype Sortiment' = Sort' [(Typ,Datensatz')]

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10

newtype Markt = Mt (Haendler -> Sortiment)
newtype Markt' = Mt' [(Haendler,Sortiment')]

data Betroffen = Betroffen | NichtBetroffen deriving (Eq,Show)

newtype Betroffene_Haendler = BH (Haendler -> Betroffen)

type AbLieferfenster = Lieferfenster


-- Aufgabe A.1

lst2fkt_la :: [(Lieferfenster,Nat0)] -> (Lieferfenster -> Nat0)
lst2fkt_la lst = error "noch nicht implementiert"

lst2fkt_so :: [(Typ,Datensatz')] -> (Typ -> Datensatz)
lst2fkt_so lst = error "noch nicht implementiert"

lst2fkt_ab :: [(Haendler,Sortiment')] -> (Haendler -> Sortiment)
lst2fkt_ab lst = error "noch nicht implementiert"

{- Knapp, aber gut nachvollziehbar, gehen die Implementierungen
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.2

lst2fkt_la' :: Lieferausblick' -> Lieferausblick
lst2fkt_la' la = error "noch nicht implementiert"

lst2fkt_so' :: Sortiment' -> Sortiment
lst2fkt_so' s = error "noch nicht implementiert"

lst2fkt_ab' :: Markt' -> Markt
lst2fkt_ab' m = error "noch nicht implementiert"

{- Knapp, aber gut nachvollziehbar, gehen die Implementierungen
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.4

preisanpassung :: Markt -> Markt
preisanpassung m = error "noch nicht implementiert"


{- Knapp, aber gut nachvollziehbar, geht die Implementierung
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.5

berichtige :: Markt -> Betroffene_Haendler -> AbLieferfenster -> Markt
berichtige m bh al = error "noch nicht implementiert"

{- Knapp, aber gut nachvollziehbar, geht die Implementierung
   Folgendermassen vor:
   ...
-}

