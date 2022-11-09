> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollstaendigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisung!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen muessen durch mindestens eine Leerzeile getrennt sein!


> type Nat0    = Int     -- Natuerliche Zahlen beginnend mit 0
> type Nat1    = Int     -- Natuerliche Zahlen beginnend mit 1
> type Nat2023 = Int     -- Natuerliche Zahlen beginnend mit 2023

> newtype EUR  = EUR { euro :: Nat1 }

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
>  show WMS { wm } = "Waschmaschinensortiment"
>  show WTS { wt } = "Waeschetrocknetsortiment"
>  show WSS { ws } = "Waescheschleudersortiment"

> data Lieferantenname = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10 deriving (Show, Eq, Enum, Bounded)

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ  


Testdaten

> t_stk_fenster2 :: Lieferfenster -> Nat0
> t_stk_fenster2 LF { quartal = q, jahr = j } = (mod j 4) + 1

> t_ds1 :: Datensatz
> t_ds1 = DS { 
>    preis_in_euro = 3, 
>    sofort_lieferbare_stueckzahl = 3, 
>    lieferbare_stueckzahl_im_Zeitfenster = t_stk_fenster2, 
>    skonto = Kein_Skonto }

> t_ds2 :: Datensatz
> t_ds2 = DS { 
>    preis_in_euro = 1, 
>    sofort_lieferbare_stueckzahl = 0, 
>    lieferbare_stueckzahl_im_Zeitfenster = t_stk_fenster2, 
>    skonto = Kein_Skonto }

> t_wts_ds1 :: Waeschetrocknertyp -> Datensatz
> t_wts_ds1 WT_Typ1 = t_ds1
> t_wts_ds1 _ = Nicht_im_Sortiment

> t_wms_ds1 :: Waschmaschinentyp -> Datensatz
> t_wms_ds1 WM_Typ1 = t_ds2
> t_wms_ds1 _ = Nicht_im_Sortiment

> t_wts1 = WTS { wt = t_wts_ds1 }
> t_wms1 = WMS { wm = t_wms_ds1 }

> t_lieferanten :: Lieferanten
> t_lieferanten L1 = t_wts1
> t_lieferanten _ = t_wms1

Testdaten examples

wm (t_lieferanten L1) WM_Typ1
lieferbare_stueckzahl_im_Zeitfenster (wm (t_lieferanten L1) WM_Typ1) LF { quartal = Q1, jahr = 2014}


Aufgabe A.1

> type Lieferantenliste = [Lieferantenname]

> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten -> Lieferantenliste
> sofort_erhaeltlich_bei s l = filter (\x -> stk_sofort_st (l x) s > 0) [L1 .. L10]

> stk_sofort_st :: Sortiment -> Suchanfrage -> Nat0
> stk_sofort_st (WMS {wm}) (WM s) = stk_sofort_ds (wm s)
> stk_sofort_st (WTS {wt}) (WT s) = stk_sofort_ds (wt s)
> stk_sofort_st (WSS {ws}) (WS s) = stk_sofort_ds (ws s)
> stk_sofort_st _ s = 0

> stk_sofort_ds :: Datensatz -> Nat0
> stk_sofort_ds DS { sofort_lieferbare_stueckzahl } = sofort_lieferbare_stueckzahl
> stk_sofort_ds Nicht_im_Sortiment = 0


 Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...


Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl = error "noch nicht implementiert"
        
Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...
 

Aufgabe A.3

> type Preis = EUR
> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten = error "noch nicht implementiert"

Knapp, aber gut nachvollziebar ,geht die Implementierung folgenderma�en vor:
... 


Aufgabe A.4

> type RabattierterPreis = EUR

> guenstigste_Lieferanten_im_Lieferfenster ::  Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> [(Lieferantenname,RabattierterPreis)]
> guenstigste_Lieferanten_im_Lieferfenster = error "noch nicht implementiert"


Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
... 
