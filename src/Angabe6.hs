module Angabe6 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1

newtype EUR  = EUR { euro :: Nat1 }

data Skonto  = Kein_Skonto 
               | DreiProzent  
               | FuenfProzent 
               | ZehnProzent deriving (Eq,Show)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq,Show,Enum)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq,Show,Enum)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq,Show,Enum)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq,Show)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show,Enum)
data Jahr          = J2023 | J2024 | J2025 deriving (Show,Ord,Eq,Enum)
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        } deriving (Eq,Show)

newtype Lieferausblick = LA (Lieferfenster -> Nat0)

instance Eq Lieferausblick where
  l1 == l2 = (wg_la l1) == (wg_la l2)

instance Show Lieferausblick where
 show l = show (wg_la l)

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment deriving (Eq)

instance Show Datensatz where
  show DS { 
    preis_in_euro = p, 
    sofort_lieferbare_stueckzahl = s, 
    lieferbare_stueckzahl_im_Zeitfenster = l, 
    skonto = sk } = "{ preis: " ++ (show p) ++ ", sofort lieferb. stk: " ++ (show s) ++ ", skonto: " ++ (show sk) ++ "}"
  show Nicht_im_Sortiment = "nicht im sortiment"
  
newtype Sortiment = Sort (Typ -> Datensatz)

instance Show Sortiment where
 show s = show (wg_so s)

instance Eq Sortiment where
  (Sort s1) == (Sort s2) = [s1 x | x <- t_liste] == [s2 x | x <- t_liste]

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq,Ord,Show,Enum)

newtype Anbieter = A (Haendler -> Sortiment)

type Suchanfrage = Typ  

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> a
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = \x -> error "Argument fehlerhaft"


-- Aufgabe A.1

f_liste :: [Lieferfenster]
f_liste = [(LF q j) | q <- [Q1 .. Q4], j <- [J2023 .. J2025]]

t_liste :: [Typ]
t_liste = [M w | w <- [M1 .. M5]] ++ [T t | t <- [T1 .. T4]] ++ [S s | s <- [S1 .. S3]] 

h_liste :: [Haendler]
h_liste = [H1 .. H10]

wg_la :: Lieferausblick -> [(Lieferfenster,Nat0)]
wg_la (LA l) = [(f, l f) | f <- f_liste] 

wg_so :: Sortiment -> [(Typ,Datensatz)]
wg_so (Sort s) = [(t, s t) | t <- t_liste]

wg_ab :: Anbieter -> [(Haendler,Sortiment)]
wg_ab (A a) = [(h, a h) | h <- h_liste]

{- Knapp, aber gut nachvollziehbar gehen die Implementierungen folgendermassen vor:
   ...
-}


-- Aufgabe A.2

instance Wgf Lieferausblick where
 ist_wgf _ = True
 wgf_fehler _ = error "Ausblickfehler"

instance Wgf Sortiment where
 ist_wgf _ = True
 wgf_fehler _ = error "Sortimentfehler"

instance Wgf Anbieter where
 ist_wgf _ = True
 wgf_fehler _ = error "Anbieterfehler"

{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}


-- Aufgabe A.5

type Haendlerliste = [Haendler]
type Stueckpreis = Nat0

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig sa a = quickSortRev [x_h | (x_h,x_s) <- wg_ab a, (fst $ sofort_lieferbar_st sa x_s) > 0]

sofort_lieferbar_st :: Suchanfrage -> Sortiment -> (Stueckzahl, Stueckpreis)
sofort_lieferbar_st sa s = foldl (\x y -> (fst x + fst y, snd x + snd y)) (0, 0) [sofort_lieferbar_ds x_ds | (x_t,x_ds) <- wg_so s, x_t == sa]

sofort_lieferbar_ds :: Datensatz -> (Stueckzahl, Stueckpreis)
sofort_lieferbar_ds (DS { sofort_lieferbare_stueckzahl = s, preis_in_euro = p }) = (s, p)
sofort_lieferbar_ds _ = (0, 0)

quickSortRev :: (Ord a) => [a] -> [a]
quickSortRev [] = []
quickSortRev (n:ns) = quickSortRev larger ++ [n] ++ quickSortRev smaller
 where 
  smaller = [m | m <- ns, m <= n]
  larger = [m | m <- ns, m > n]

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl sa a = foldl (\(acc_s, acc_p) (x_s, x_p) -> (x_s + acc_s, (x_s * x_p) + acc_p)) (0, 0) [sofort_lieferbar_st sa x_s | (_,x_s) <- wg_ab a]

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.7

type Preis = EUR
type ProduktDaten = (Stueckzahl, Stueckpreis, Skonto)
type HaendlerDaten = (Stueckzahl, Stueckpreis, Haendler)
type Skontieren = Bool
type MinStueckzahl = Stueckzahl

guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten sa f a = vielleicht $ quickSortRev $ map (\(_,_,h) -> h) $ guenstigste sa f a False 0  
 where
  vielleicht :: Haendlerliste -> Maybe Haendlerliste 
  vielleicht x = if x == [] then Nothing else Just x 
 
guenstigste :: Suchanfrage -> Lieferfenster -> Anbieter -> Skontieren -> MinStueckzahl -> [HaendlerDaten]
guenstigste sa f a skontieren min_stk = foldl finde_guenstigste [] produktinfo_a
 where
  finde_guenstigste :: [HaendlerDaten] -> HaendlerDaten -> [HaendlerDaten]
  finde_guenstigste [] y@(y_s, _, _) = if y_s > 0 then [y] else []
  finde_guenstigste list@((_, x_p, _):_) y@(y_s, y_p, _)
   | y_s == 0 || (y_p > x_p) = list
   | (y_p == x_p) = list ++ [y]
   | True = [y]
   
  produktinfo_a :: [HaendlerDaten]  
  produktinfo_a = [transform h (produktinfo_st st) | (h, st) <- wg_ab a]
  
  produktinfo_st :: Sortiment -> ProduktDaten
  produktinfo_st s = if l == [] then (0, 0, Kein_Skonto) else head l
   where
    l :: [ProduktDaten] 
    l = filter (\(s, _, _) -> s >= min_stk) [produktinfo_ds x_ds | (x_t, x_ds) <- wg_so s, x_t == sa]
     
  produktinfo_ds :: Datensatz -> ProduktDaten
  produktinfo_ds Nicht_im_Sortiment = (0, 0, Kein_Skonto)
  produktinfo_ds (DS { preis_in_euro = p, lieferbare_stueckzahl_im_Zeitfenster = lst, skonto = sk }) = (stk, p, sk)
   where stk = stk_ausblick lst
     
  stk_ausblick :: Lieferausblick -> Stueckzahl
  stk_ausblick la = if l == [] then 0 else head l
   where
    l :: [Nat0] 
    l = [x_p | (x_f, x_p) <- wg_la la, x_f == f]

  transform :: Haendler -> ProduktDaten -> HaendlerDaten
  transform h (stk, p, sk) = (stk, (if skontieren then skontiert (p * min_stk) sk else p), h)

  skontiert :: Stueckpreis -> Skonto -> Gesamtpreis
  skontiert p DreiProzent = runden $ (fromIntegral p) * 0.97
  skontiert p FuenfProzent = runden $ (fromIntegral p) * 0.95
  skontiert p ZehnProzent = runden $ (fromIntegral p) * 0.9
  skontiert p _ = p
  
  runden :: (Integral b) => Double -> b
  runden x = 10 * (ceiling (x / 10))

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster _ _ 0 _ = []  
guenstigste_Lieferanten_im_Lieferfenster sa f min_stk a = quickSortRevTuple $ map (\(_, p, h) -> (h, (EUR p))) $ guenstigste sa f a True min_stk  

quickSortRevTuple :: [(Haendler,RabattierterPreis)] -> [(Haendler,RabattierterPreis)]
quickSortRevTuple [] = []
quickSortRevTuple (n@(n_h,_):ns) = quickSortRevTuple larger ++ [n] ++ quickSortRevTuple smaller
 where 
  smaller = [m | m@(h,_) <- ns, h <= n_h]
  larger = [m | m@(h,_) <- ns, h > n_h]

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}

