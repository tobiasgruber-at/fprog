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
>                | ZehnProzent

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3

> data Typ = WM Waschmaschinentyp
>            | WT Waeschetrocknertyp
>            | WS Waescheschleudertyp

> data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
> type Jahr          = Nat2023
> data Lieferfenster = LF { quartal :: Quartal,
>                           jahr    :: Jahr 
>                         }

> data Datensatz 
>   = DS { preis_in_euro :: Nat1,
>          sofort_lieferbare_stueckzahl :: Nat0,
>          lieferbare_stueckzahl_im_Zeitfenster :: Lieferfenster -> Nat0,
>          skonto :: Skonto
>        }
>     | Nicht_im_Sortiment

> data Sortiment 
>   = WMS {wm   :: Waschmaschinentyp   -> Datensatz}
>     | WTS {wt :: Waeschetrocknertyp  -> Datensatz}
>     | WSS {ws :: Waescheschleudertyp -> Datensatz}

> data Lieferantenname = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ  


Aufgabe A.1

> type Lieferantenliste = [Lieferantenname]

> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten  -> Lieferantenliste
> sofort_erhaeltlich_bei = error "noch nicht implementiert"

> test1 :: Lieferfenster -> Quartal
> test1 LF { quartal, jahr } = quartal

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
