module Angabe5 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 }

data Skonto  = Kein_Skonto
               | DreiProzent  
               | FuenfProzent
               | ZehnProzent deriving (Eq, Show)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq, Show)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq, Show)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq, Show)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq, Show)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        } deriving (Eq, Show)

newtype Lieferausblick = LA [(Lieferfenster,Nat0)] deriving (Eq, Show)

data Datensatz
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment deriving (Eq, Show)

newtype Sortiment = Sort [(Typ,Datensatz)] deriving (Eq, Show)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq, Show)

newtype Anbieter = A [(Haendler,Sortiment)] deriving (Show)

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

instance Wgf Lieferausblick where 
 ist_wgf (LA l) = [a | a@(a_f,a_n) <- l, [b | b@(b_f,b_n) <- l, b_f == a_f && b_n /= a_n] /= []] == []
 wgf_fehler _ = error "Ausblickfehler"

instance Wgf Sortiment where
 ist_wgf (Sort l) = [a | a@(a_t,a_ds) <- l, (not (ds_s_wgf a_ds)) || length [b | b@(b_t,_) <- l, b_t == a_t] > 1] == []
 wgf_fehler _ = error "Sortimentfehler"

ds_s_wgf :: Datensatz -> Bool
ds_s_wgf (DS { lieferbare_stueckzahl_im_Zeitfenster = la }) = ist_wgf la
ds_s_wgf _ = True 

instance Wgf Anbieter where
 ist_wgf (A l) = [a | a@(a_h,a_s) <- l, (not (ist_wgf a_s)) || length [b | b@(b_h,_) <- l, b_h == a_h] > 1] == []
 wgf_fehler _ = error "Anbieterfehler"

{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}


-- Aufgabe A.5

type Haendlerliste = [Haendler]
type Stueckpreis = Nat0

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig sa a = [x_h | (x_h,x_s) <- anbieter_liste(validiere a), (fst $ sofort_lieferbar_st sa x_s) > 0]

sofort_lieferbar_st :: Suchanfrage -> Sortiment -> (Stueckzahl, Stueckpreis)
sofort_lieferbar_st sa (Sort s_l) = foldl (\x y -> (fst x + fst y, snd x + snd y)) (0, 0) [sofort_lieferbar_ds x_ds | (x_t,x_ds) <- s_l, x_t == sa]

sofort_lieferbar_ds :: Datensatz -> (Stueckzahl, Stueckpreis)
sofort_lieferbar_ds (DS { sofort_lieferbare_stueckzahl = s, preis_in_euro = p }) = (s, p)
sofort_lieferbar_ds _ = (0, 0)

anbieter_liste :: Anbieter -> [(Haendler,Sortiment)]
anbieter_liste (A l) = l

validiere :: (Wgf a) => a -> a
validiere a
 | ist_wgf a == True = a
 | otherwise = wgf_fehler a 

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl sa a = foldl (\(acc_s, acc_p) (x_s, x_p) -> (x_s + acc_s, (x_s * x_p) + acc_p)) (0, 0) [sofort_lieferbar_st sa x_s | (_,x_s) <- anbieter_liste(validiere_anbieter a)]

validiere_anbieter :: (Wgf a) => a -> a
validiere_anbieter a
 | ist_wgf a == True = a
 | otherwise = error "Anbieterargumentfehler"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.7

type Preis = EUR
-- guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
-- guenstigste_Lieferanten = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR

-- guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
-- guenstigste_Lieferanten_im_Lieferfenster = error "Noch nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}

