module Ville where 

import Zone 
import Citoyen
import Types
import Forme
import Data.Map as Map
import Occupation
data Ville = Ville {
    villeZones :: Map ZonId Zone,
    villeCitoyens :: Map CitId Citoyen
}

-- Création de quelques zones
zone1 = Eau (Rectangle (C 0 0) 10 5)
zone2 = Route (Rectangle (C 10 0) 5 10)
zone3 = ZoneResidentielle (Rectangle (C 0 5) 10 10) []
zone4 = ZoneIndustrielle (Rectangle (C 10 10) 10 5) []
zone5 = ZoneCommerciale (Rectangle (C 0 15) 5 10) []

-- Création de quelques citoyens
citoyen1 = Immigrant (C 1 1) (1, 1, 1) Travailler
citoyen2 = Habitant (C 3 3) (2, 2, 2) (BatId 1, Nothing, Nothing) Dormir
citoyen3 = Emigrant (C 5 5) FaireCourses

-- Création de la ville
maVille = Ville {
    villeZones = Map.fromList [(ZonId 1, zone1), (ZonId 2, zone2), (ZonId 3, zone3), (ZonId 4, zone4), (ZonId 5, zone5)],
    villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
}

instance Show Ville where
    show ville =
        "Ville {\n" ++
        "  Zones:\n" ++
        zonesToString (Map.toList (villeZones ville)) ++
        "\n  Citoyens:\n" ++
        citoyensToString (Map.toList (villeCitoyens ville)) ++
        "\n}"

zonesToString :: [(ZonId, Zone)] -> String
zonesToString [] = ""
zonesToString ((zonId, zone):rest) =
    "    " ++ show zonId ++ ": " ++ show zone ++ "\n" ++ zonesToString rest

citoyensToString :: [(CitId, Citoyen)] -> String
citoyensToString [] = ""
citoyensToString ((citId, citoyen):rest) =
    "    " ++ show citId ++ ": " ++ show citoyen ++ "\n" ++ citoyensToString rest
