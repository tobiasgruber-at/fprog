> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollstaendigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisung!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen muessen durch mindestens eine Leerzeile getrennt sein!


> type Nat0    = Int     -- Natuerliche Zahlen beginnend mit 0
> type Nat1    = Int     -- Natuerliche Zahlen beginnend mit 1
> type Nat2023 = Int     -- Natuerliche Zahlen beginnend mit 2023

> newtype EUR  = EUR { euro :: Nat1 } deriving (Eq, Ord, Show)

> data Skonto  = Kein_Skonto 
>                | DreiProzent  
>                | FuenfProzent 
>                | ZehnProzent deriving (Show)

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5 deriving (Show, Enum, Bounded)
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4 deriving (Show, Enum, Bounded)
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3 deriving (Show, Enum, Bounded)

> data Typ = WM Waschmaschinentyp
>            | WT Waeschetrocknertyp
>            | WS Waescheschleudertyp deriving (Show)

> data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
> type Jahr          = Nat2023
> data Lieferfenster = LF { quartal :: Quartal,
>                           jahr    :: Jahr 
>                         } deriving (Show)

> data Datensatz 
>   = DS { preis_in_euro :: Nat1,
>          sofort_lieferbare_stueckzahl :: Nat0,
>          lieferbare_stueckzahl_im_Zeitfenster :: Lieferfenster -> Nat0,
>          skonto :: Skonto
>        }
>     | Nicht_im_Sortiment

> instance Show Datensatz where
>  show DS { 
>   preis_in_euro = p, 
>   sofort_lieferbare_stueckzahl = s, 
>   lieferbare_stueckzahl_im_Zeitfenster = l, 
>   skonto = sk } = "{ preis: " ++ (show p) ++ ", sofort lieferb. stk: " ++ (show s) ++ ", skonto: " ++ (show sk) ++ "}"
>  show Nicht_im_Sortiment = "nicht im sortiment"

> data Sortiment 
>   = WMS {wm   :: Waschmaschinentyp   -> Datensatz}
>     | WTS {wt :: Waeschetrocknertyp  -> Datensatz}
>     | WSS {ws :: Waescheschleudertyp -> Datensatz}

> instance Show Sortiment where
>  show (WMS wm) = "Waschmaschinensortiment"
>  show (WTS wt) = "Waeschetrocknetsortiment"
>  show (WSS ws) = "Waescheschleudersortiment"

> data Lieferantenname = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10 deriving (Show, Eq, Enum, Bounded, Ord)

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ  


Aufgabe A.1

> type Lieferantenliste = [Lieferantenname]

> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten -> Lieferantenliste
> sofort_erhaeltlich_bei s l = filter (\x -> fst (stk_sofort_st s (l x)) > 0) lieferanten

> lieferanten :: Lieferantenliste
> lieferanten = [L1 .. L10]

> stk_sofort_st :: Suchanfrage -> Sortiment -> (Stueckzahl, Gesamtpreis)
> stk_sofort_st (WM s) (WMS wm) = stk_sofort_ds (wm s)
> stk_sofort_st (WT s) (WTS wt) = stk_sofort_ds (wt s)
> stk_sofort_st (WS s) (WSS ws) = stk_sofort_ds (ws s)
> stk_sofort_st _ s = (0, 0)

> stk_sofort_ds :: Datensatz -> (Stueckzahl, Gesamtpreis)
> stk_sofort_ds DS { sofort_lieferbare_stueckzahl = s, preis_in_euro = p } = ( s, s * p )
> stk_sofort_ds _ = (0, 0)

 Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...


Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl s l = foldl (\x y -> (fst x + fst y, snd x + snd y)) (0, 0) lieferanten_daten
>  where lieferanten_daten = (map (\x -> stk_sofort_st s (l x)) lieferanten)

Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...
 

Aufgabe A.3

> type Preis = EUR
> type Skontieren = Bool
> type ProduktDaten = (Stueckzahl, Preis, Skonto)
> type LieferantDaten = (Lieferantenname, Stueckzahl, Preis)

> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten s f l 
>  | length g == 0 = Nothing
>  | True = Just (map (\(n, _, _) -> n) g)
>  where g = guenstigste s f l False

> guenstigste :: Suchanfrage -> Lieferfenster -> Lieferanten -> Skontieren -> [LieferantDaten]
> guenstigste s f l skontieren = foldl finde_guenstigste [] daten
>  where
>   finde_guenstigste :: [LieferantDaten] -> LieferantDaten -> [LieferantDaten]
>   finde_guenstigste [] y@(_, y_s, _) = if y_s > 0 then [y] else []
>   finde_guenstigste list@(x@(_, _, x_p):_) y@(_, y_s, y_p)
>    | y_s == 0 || (y_p > x_p) = list
>    | (y_p == x_p) = list ++ [y]
>    | True = [y]
>   daten :: [LieferantDaten]
>   daten = map (\x -> transform x (produktinfo_fenster_st s f (l x))) lieferanten
>   transform :: Lieferantenname -> ProduktDaten -> LieferantDaten
>   transform x (stk, p, skonto) = (x, stk, (if skontieren then skontiert p skonto else p))

> produktinfo_fenster_st :: Suchanfrage -> Lieferfenster -> Sortiment -> ProduktDaten
> produktinfo_fenster_st (WM s) f (WMS wm) = produktinfo_fenster_ds f (wm s)
> produktinfo_fenster_st (WT s) f (WTS wt) = produktinfo_fenster_ds f (wt s)
> produktinfo_fenster_st (WS s) f (WSS ws) = produktinfo_fenster_ds f (ws s)
> produktinfo_fenster_st _ _ _ = (0, EUR 0, Kein_Skonto)

> produktinfo_fenster_ds :: Lieferfenster -> Datensatz -> ProduktDaten
> produktinfo_fenster_ds f (DS p _ lz skonto) = (lz f, EUR p, skonto)
> produktinfo_fenster_ds _ _ = (0, EUR 0, Kein_Skonto)

> skontiert :: Preis -> Skonto -> Preis
> skontiert (EUR euro) DreiProzent = EUR (ceiling ((fromIntegral euro) * 0.97))
> skontiert (EUR euro) FuenfProzent = EUR (ceiling ((fromIntegral euro) * 0.95))
> skontiert (EUR euro) ZehnProzent = EUR (ceiling ((fromIntegral euro) * 0.9))
> skontiert p _ = p

Knapp, aber gut nachvollziebar ,geht die Implementierung folgenderma�en vor:
... 


Aufgabe A.4

> type RabattierterPreis = EUR

> guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> [(Lieferantenname,RabattierterPreis)]
> guenstigste_Lieferanten_im_Lieferfenster s f stk l = map (\(n, _, p) -> (n, p)) g
>  where g = filter (\x@(_, x_stk, _) -> x_stk >= stk) (guenstigste s f l True)

Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
... 
