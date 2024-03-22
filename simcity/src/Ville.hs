module Ville where 

import Zone 
import Citoyen
import Types
import Forme
import Data.Map as Map
import Occupation

-- données
data Ville = Ville {
    villeZones :: Map ZonId Zone,
    villeCitoyens :: Map CitId Citoyen
}

-- instance de Ville
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

-- -- Création de quelques zones
-- zone1 = Eau (Rectangle (C 0 0) 10 5)
-- zone2 = Route (Rectangle (C 10 0) 5 10)
-- zone3 = ZoneResidentielle (Rectangle (C 0 5) 10 10) []
-- zone4 = ZoneIndustrielle (Rectangle (C 10 10) 10 5) []
-- zone5 = ZoneCommerciale (Rectangle (C 0 15) 5 10) []

-- Création de quelques citoyens
citoyen1 = Immigrant (C 1 1) (1, 1, 1) Travailler
citoyen2 = Habitant (C 3 3) (2, 2, 2) (BatId 1, Nothing, Nothing) Dormir
citoyen3 = Emigrant (C 5 5) FaireCourses

-- -- Création de la ville
-- maVille = Ville {
--     villeZones = Map.fromList [(ZonId 1, zone1), (ZonId 2, zone2), (ZonId 3, zone3), (ZonId 4, zone4), (ZonId 5, zone5)],
--     villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
-- }
-- >>> print maVille

-- >>> :t maVille
-- maVille :: Ville
-- invariant 
invariantVille :: Ville -> Bool
invariantVille  v=  
            invariantZonesDisjointes v &&
            villeVerifiantAdjacenceARoute v

--  toutes les zones soit disjointes deux `a deux
-- on écrit une fonction zonesDisjointes de Zone -> Zone -> Bool pour vérifier que deux zones sont disjointes 
-- on rappele que deux zones sont disjointes si elles n'ont pas de collision
--et puis  on utilise cette  fonction(zonesDisjointes) et on map sur toutes les zones de la ville pour vérifier que toutes les zones sont disjointes deux à deux 
zonesDisjointes :: Zone -> Zone -> Bool
zonesDisjointes z1 z2 =
    not (collision  (zoneForme z1) (zoneForme z2))
invariantZonesDisjointes :: Ville -> Bool
invariantZonesDisjointes ville =
    all (\(zonId1, zone1) -> all (\(zonId2, zone2) -> zonId1 == zonId2 || zonesDisjointes zone1 zone2) (Map.toList (villeZones ville))) (Map.toList (villeZones ville))

-- zoneA = Eau (Rectangle (C 0 0) 1 1)
-- zoneB = Route (Rectangle (C 2 2) 1 1)
-- zoneC = ZoneResidentielle (Rectangle (C 4 4) 1 1) []
-- zoneD = ZoneIndustrielle (Rectangle (C 6 6) 1 1) []
-- zoneE = ZoneCommerciale (Rectangle (C 8 8) 1 1) []



-- Création de la ville
-- maVille2 = Ville {
--     villeZones = Map.fromList [(ZonId 1, zoneA), (ZonId 2, zoneB), (ZonId 3, zoneC), (ZonId 4, zoneD), (ZonId 5, zoneE)],
--     villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
-- }

-- >>> invariantZonesDisjointes maVille
-- False
-- >>> invariantZonesDisjointes maVille2
-- True

-- Fonction pour vérifier si une zone est adjacente à au moins une zone de route
estAdjacenteARoute :: Zone -> Ville -> Bool
estAdjacenteARoute zone ville =
    any (\(_, z) -> case z of
                      Route _ -> True
                      _       -> False) (Map.toList (villeZones ville))

-- Fonction pour vérifier si toutes les zones vérifient l'adjacence à une zone de route
villeVerifiantAdjacenceARoute :: Ville -> Bool
villeVerifiantAdjacenceARoute ville =
    all (\(_, z) -> case z of
                      Route _ -> True -- Les zones de route sont toujours adjacentes à une zone de route
                      _       -> estAdjacenteARoute z ville) (Map.toList (villeZones ville))

-- -- Création de zones pour la première ville
-- zoneA = Eau (Rectangle (C 0 0) 1 1)
-- zoneB = Route (Rectangle (C 2 0) 1 1)
-- zoneC = ZoneResidentielle (Rectangle (C 4 0) 1 1) []
-- zoneD = ZoneIndustrielle (Rectangle (C 0 2) 1 1) []
-- zoneE = ZoneCommerciale (Rectangle (C 0 4) 1 1) []

-- -- Création de la première ville
-- ville1 = Ville {
--     villeZones = Map.fromList [(ZonId 1, zoneA), (ZonId 2, zoneB), (ZonId 3, zoneC), (ZonId 4, zoneD), (ZonId 5, zoneE)],
--     villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
-- }


zoneA = Eau (Rectangle (C 0 0) 1 1)
zoneB = Route (Rectangle (C 2 0) 1 1)
zoneC = ZoneResidentielle (Rectangle (C 4 0) 1 1) []
zoneD = ZoneIndustrielle (Rectangle (C 0 2) 1 1) []
zoneE = ZoneCommerciale (Rectangle (C 0 4) 1 1) []

-- Création de la première ville
villeExemple = Ville {
    villeZones = Map.fromList [(ZonId 1, zoneA), (ZonId 2, zoneB), (ZonId 3, zoneC), (ZonId 4, zoneD), (ZonId 5, zoneE)],
    villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
}
