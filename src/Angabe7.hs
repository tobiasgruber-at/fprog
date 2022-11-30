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
               | ZehnProzent deriving (Show)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq,Enum)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq,Enum)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq,Enum)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        } deriving (Eq,Show)

newtype Lieferausblick  = LA (Lieferfenster -> Nat0)
newtype Lieferausblick' = LA' [(Lieferfenster,Nat0)]

instance Show Lieferausblick where
 show l = "Lieferausblick"

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment deriving (Show)

data Datensatz' 
   = DS' { preis_in_euro' :: Nat1,
           sofort_lieferbare_stueckzahl' :: Nat0,
           lieferbare_stueckzahl_im_Zeitfenster' :: Lieferausblick',
           skonto' :: Skonto
        }
     | Nicht_im_Sortiment'

newtype Sortiment  = Sort (Typ -> Datensatz)
newtype Sortiment' = Sort' [(Typ,Datensatz')]

instance Show Sortiment where
 show s = "Sortiment"

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq,Enum)

newtype Markt = Mt (Haendler -> Sortiment)
newtype Markt' = Mt' [(Haendler,Sortiment')]

instance Show Markt where
 show m = "Markt"

data Betroffen = Betroffen | NichtBetroffen deriving (Eq,Show)

newtype Betroffene_Haendler = BH (Haendler -> Betroffen)

type AbLieferfenster = Lieferfenster


-- Aufgabe A.1

instance Show (Haendler -> Sortiment) where
 show s = "Haendler -> Sortiment"
 
instance Show (Typ -> Datensatz) where
 show s = "Typ -> Datensatz"

instance Show (Lieferfenster -> Nat0) where
 show s = "Lieferfenster -> Nat0"

change :: Eq a => (a -> b) -> a -> b -> (a -> b)
change f x y = g where g = \z -> if z == x then y else f z

convert :: Eq a => [(a, b)] -> (a -> b) -> (a -> b)
convert [] f = f 
convert ((lf, n):xs) f = convert xs (change f lf n)

default_fn = \_ -> error "undefiniert"

-- lst2fkt_la [((LF Q1 2023), 2)] $ (LF Q1 2023)
-- > 2
-- lst2fkt_la [((LF Q1 2023), 2)] $ (LF Q1 2024)
-- > error
lst2fkt_la :: [(Lieferfenster,Nat0)] -> (Lieferfenster -> Nat0)
lst2fkt_la lst = convert lst default_fn 

-- lst2fkt_so [(M M1, DS' 1 1 (LA' [((LF Q1 2023), 2)]) Kein_Skonto)] (M M1)
-- > DS {preis_in_euro = 1, sofort_lieferbare_stueckzahl = 1, lieferbare_stueckzahl_im_Zeitfenster = Lieferausblick, skonto = Kein_Skonto}
lst2fkt_so :: [(Typ,Datensatz')] -> (Typ -> Datensatz)
lst2fkt_so lst = convert (transform lst []) default_fn 
 where
  transform :: [(Typ,Datensatz')] -> [(Typ,Datensatz)] -> [(Typ,Datensatz)]
  transform [] l = l
  transform ((t,Nicht_im_Sortiment'):xs) l = transform xs ((t,Nicht_im_Sortiment):l)
  transform ((t,ds):xs) l = transform xs ((t, transform_ds ds):l)
  transform_ds :: Datensatz' -> Datensatz 
  transform_ds (DS' { preis_in_euro'=p, sofort_lieferbare_stueckzahl'=stk, lieferbare_stueckzahl_im_Zeitfenster'=(LA' la), skonto'=sk }) = DS p stk (LA $ lst2fkt_la la) sk

--  lst2fkt_ab [(H1, (Sort' [(M M1, DS' 1 1 (LA' [((LF Q1 2023), 2)]) Kein_Skonto)]))] $ H1
lst2fkt_ab :: [(Haendler,Sortiment')] -> (Haendler -> Sortiment)
lst2fkt_ab lst = convert (transform lst []) default_fn 
 where
  transform :: [(Haendler,Sortiment')] -> [(Haendler,Sortiment)] -> [(Haendler,Sortiment)]
  transform [] l = l
  transform ((h,s):xs) l = transform xs ((h, transform_s s):l) 
  transform_s :: Sortiment' -> Sortiment
  transform_s (Sort' ls) = (Sort $ lst2fkt_so ls)

{- Knapp, aber gut nachvollziehbar, gehen die Implementierungen
   Folgendermassen vor:
   ...
-}

-- Aufgabe A.2

lst2fkt_la' :: Lieferausblick' -> Lieferausblick
lst2fkt_la' (LA' l) = (LA $ lst2fkt_la l)

lst2fkt_so' :: Sortiment' -> Sortiment
lst2fkt_so' (Sort' l) = (Sort $ lst2fkt_so l)

lst2fkt_ab' :: Markt' -> Markt
lst2fkt_ab' (Mt' l) = (Mt $ lst2fkt_ab l)

{- Knapp, aber gut nachvollziehbar, gehen die Implementierungen
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.4

preis_m1 :: Markt -> Haendler -> Typ -> Nat0
preis_m1 (Mt m) h t = preis_s1 (m h) t

preis_s1 :: Sortiment -> Typ -> Nat1
preis_s1 (Sort f) t = preis_ds1 (f t) 

preis_ds1 :: Datensatz -> Nat1
preis_ds1 Nicht_im_Sortiment = error "nicht angeboten"
preis_ds1 (DS { preis_in_euro = p }) = p


-- preis_m1 (preisanpassung (Mt (\x->(if x == H1 then (Sort (\y -> (DS 1 2 (LA (\z -> 3)) Kein_Skonto))) else (Sort (\y -> (DS 3 2 (LA (\z -> 3)) Kein_Skonto))))))) H4 (M M1)
-- 1
-- preis_m1 (preisanpassung (Mt (\x->(if x == H1 then (Sort (\y -> (DS 5 2 (LA (\z -> 3)) Kein_Skonto))) else (Sort (\y -> (DS 3 2 (LA (\z -> 3)) Kein_Skonto))))))) H1 (M M1)
-- 3
preisanpassung :: Markt -> Markt
preisanpassung m = pa m $ [M w | w <- [M1 .. M5]] ++ [T t | t <- [T1 .. T4]] ++ [S s | s <- [S1 .. S3]]
 where 
    pa :: Markt -> [Typ] -> Markt 
    pa m [] = m 
    pa m (t:ts) = pa (if (guenstigster_pm m t) < 0 then m else (anpassung_t m t (guenstigster_pm m t) [H1 .. H10])) ts 

-- anpassung_t (Mt (\x->(if x == H1 then (Sort (\y -> Nicht_im_Sortiment)) else (Sort (\y -> Nicht_im_Sortiment))))) (M M2) 4
anpassung_t :: Markt -> Typ -> Nat0 -> [Haendler] -> Markt
anpassung_t m _ _ [] = m
anpassung_t (Mt m) t p (h:hs) = anpassung_t (Mt (change m h (anpassung_s (m h)))) t p hs
 where
  anpassung_s :: Sortiment -> Sortiment
  anpassung_s (Sort s) = (Sort $ change s t (anpassung_ds (s t)))
  anpassung_ds :: Datensatz -> Datensatz
  anpassung_ds Nicht_im_Sortiment = Nicht_im_Sortiment
  anpassung_ds (DS { sofort_lieferbare_stueckzahl=stk, lieferbare_stueckzahl_im_Zeitfenster=ls, skonto=sk }) = (DS p stk ls sk)
   
-- guenstigster_pm (Mt (\x->(if x == H1 then (Sort (\y -> (DS 1 2 (LA (\z -> 3)) Kein_Skonto))) else (Sort (\y -> (DS 2 2 (LA (\z -> 3)) Kein_Skonto)))))) (M M2)
guenstigster_pm :: Markt -> Typ -> Nat1  
guenstigster_pm (Mt f) t = if length l == 0 then -1 else foldr min (head l) l
 where l = filter (\x -> x >= 0) [preis_s (f h) t | h <- [H1 .. H10]] 

preis_s :: Sortiment -> Typ -> Nat1
preis_s (Sort f) t = preis_ds (f t) 

preis_ds :: Datensatz -> Nat1
preis_ds Nicht_im_Sortiment = -1
preis_ds (DS { preis_in_euro = p }) = p

preis :: Datensatz -> Nat0
preis (DS { preis_in_euro = p }) = p

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

