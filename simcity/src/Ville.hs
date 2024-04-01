{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Ville where

import Zone
import Citoyen
import Types
import Forme
import Data.Map as Map
import Occupation
import Batiment

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
            && routesConnexes v

--  toutes les zones soit disjointes deux `a deux
-- on écrit une fonction zonesDisjointes de Zone -> Zone -> Bool pour vérifier que deux zones sont disjointes 
-- on rappele que deux zones sont disjointes si elles n'ont pas de collision
--et puis  on utilise cette  fonction(zonesDisjointes) et on map sur toutes les zones de la ville pour vérifier que toutes les zones sont disjointes deux à deux 

invariantZonesDisjointes :: Ville -> Bool
invariantZonesDisjointes ville =
    all (\(zonId1, zone1) -> all (\(zonId2, zone2) -> zonId1 == zonId2 || zonesDisjointes zone1 zone2) (Map.toList (villeZones ville))) (Map.toList (villeZones ville))

-- zoneA = Eau (Rectangle (C 0 0) 1 1)
-- zoneB = Route (Rectangle (C 2 2) 1 1)
-- zoneC = ZoneResidentielle (Rectangle (C 4 4) 1 1) []
-- zoneD = ZoneIndustrielle (Rectangle (C 6 6) 1 1) []
-- zoneE = ZoneCommerciale (Rectangle (C 8 8) 1 1) []



-- -- Création de la ville
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


-- zoneA = Eau (Rectangle (C 0 0) 1 1)
-- zoneB = Route (Rectangle (C 2 0) 1 1)
-- zoneC = ZoneResidentielle (Rectangle (C 4 0) 1 1) []
-- zoneD = ZoneIndustrielle (Rectangle (C 0 2) 1 1) []
-- zoneE = ZoneCommerciale (Rectangle (C 0 4) 1 1) []

-- -- Création de la première ville
-- villeExemple = Ville {
--     villeZones = Map.fromList [(ZonId 1, zoneA), (ZonId 2, zoneB), (ZonId 3, zoneC), (ZonId 4, zoneD), (ZonId 5, zoneE)],
--     villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
-- }
-- Vérifie si une liste de zones est une séquence de zones de route connexes
estRouteConnexe :: [Zone] -> Ville -> Bool
estRouteConnexe [] _ = True
estRouteConnexe [_] _ = True
estRouteConnexe (z1:z2:zs) ville = estAdjacenteARoute z1 ville && estRouteConnexe (z2:zs) ville

-- Vérifie si les routes sont connexes dans la ville
routesConnexes :: Ville -> Bool
routesConnexes ville =
    let zonesRoute = Map.filter (\z -> case z of Route _ -> True; _ -> False) (villeZones ville)
        listZonesRoute = Map.elems zonesRoute
    in
        estRouteConnexe listZonesRoute ville

-- -- Création de zones pour la ville
-- zoneA = Eau (Rectangle (C 0 0) 1 1)
-- zoneB = Route (Rectangle (C 2 0) 1 1)
-- zoneC = Route (Rectangle (C 4 0) 1 1)
-- zoneD = Route (Rectangle (C 6 0) 1 1)
-- zoneE = Eau (Rectangle (C 0 2) 1 1)
-- zoneF = Route (Rectangle (C 2 2) 1 1)
-- zoneG = Eau (Rectangle (C 4 2) 1 1)
-- zoneH = Route (Rectangle (C 6 2) 1 1)

-- -- Création de la ville
-- v = Ville {
--     villeZones = Map.fromList [(ZonId 1, zoneA), (ZonId 2, zoneB), (ZonId 3, zoneC), (ZonId 4, zoneD),
--                                (ZonId 5, zoneE), (ZonId 6, zoneF), (ZonId 7, zoneG), (ZonId 8, zoneH)],
--     villeCitoyens = Map.empty
-- }


-- construire une zone dans une ville 
-- Fonction pour ajouter une zone à une ville
construit :: Ville -> Zone -> Ville
construit ville zone = Ville { villeZones = Map.insertWith (\new old -> old) (prochainZonId ville) zone (villeZones ville), villeCitoyens = villeCitoyens ville }


-- Fonction pour générer le prochain identifiant de zone
prochainZonId :: Ville -> ZonId
prochainZonId ville =
    let maxId = if Map.null (villeZones ville) then ZonId 0 else maximum (Map.keys (villeZones ville))
    in ZonId (unZonId maxId + 1)

unZonId :: ZonId -> Int
unZonId (ZonId i) = i

-- Précondition pour la fonction construit
-- cette précondition vérifie que la zone à ajouter est valide
-- on a pas a reverfier que la zone écrase une autre zone car on utilise l'invariant qui vérifie qu'il n'y a pas de collision entre les zones
preconditionConstruit :: Ville -> Zone -> Bool
preconditionConstruit ville zone =
    estZoneValide zone && invariantVille (construit ville zone)

-- post condition 
-- cette post condition vérifier que la taille de la ville après l'ajout d'une zone est égale à la taille de la ville avant l'ajout d'une zone plus 1
postconditionConstruit :: Ville -> Zone -> Ville -> Bool
postconditionConstruit ville zone villeApres =
    invariantVille villeApres && Map.size (villeZones villeApres) == Map.size (villeZones ville) + 1






-- ! Batiments 


-- ! Créer un batiment

construireBatiment :: Batiment -> Zone -> ZonId -> Ville -> Ville
construireBatiment batiment zone zonId ville =
    let formeZone = zoneForme zone
        nouvelleZone = case zone of
            Eau _ -> Eau formeZone
            Route _ -> Eau formeZone
            ZoneResidentielle _ batiments -> ZoneResidentielle formeZone (batiment : batiments)
            ZoneIndustrielle _ batiments -> ZoneIndustrielle formeZone (batiment : batiments)
            ZoneCommerciale _ batiments -> ZoneCommerciale formeZone (batiment : batiments)
            Admin _ _ -> Admin formeZone batiment
        zonesMisesAJour = remplacerZone zonId nouvelleZone (villeZones ville)
    in ville { villeZones = zonesMisesAJour }


-- rajouter le batiment dans la zeone
remplacerZone :: ZonId -> Zone -> Map ZonId Zone -> Map ZonId Zone
remplacerZone = Map.insert

-- ville = Ville {
--   villeZones = Map.fromList [(ZonId 1, zone1), (ZonId 2, zone2)],
--   villeCitoyens = Map.empty
-- }

-- zone1 = ZoneResidentielle (Rectangle (C 0 0) 10 10) []
-- zone2 = Route (Rectangle (C 10 0) 5 10)

-- batiment = Cabane {
--   forme = Rectangle (C 5 5) 3 3,
--   zoneId = ZonId 1,
--   entree = C 5 8,
--   capacite = 10,
--   habitants = []
-- }

-- nouvelleVille = construireBatiment batiment zone1 (ZonId 1) ville

-- >>>invariantVille nouvelleVille
-- True

-- >>> show nouvelleVille
-- "Ville {\n  Zones:\n    ZonId 1: Zone r\233sidentielle Rectangle (C {cx = 0, cy = 0}) 10 10 avec 1 b\226timents\n    ZonId 2: Route Rectangle (C {cx = 10, cy = 0}) 5 10\n\n  Citoyens:\n\n}"

-- : pré/post/inv des batiments

precondition_ConstruitBatiment :: Ville -> Zone  -> Batiment -> Bool
precondition_ConstruitBatiment v z  b =
    zoneConstructionCorrect z &&
     not (batimentDejaPresent v z b )
zoneConstructionCorrect :: Zone -> Bool
zoneConstructionCorrect z =
    case z of
        ZoneResidentielle _ _ -> True
        ZoneIndustrielle _ _ -> True
        ZoneCommerciale _ _ -> True
        Admin _ _ -> True
        _ -> False

batimentDejaPresent :: Ville -> Zone -> Batiment -> Bool
batimentDejaPresent ville zone batiment =
    case zone of
        ZoneResidentielle _ batiments -> batimentDansListe batiments batiment
        ZoneIndustrielle _ batiments -> batimentDansListe batiments batiment
        ZoneCommerciale _ batiments -> batimentDansListe batiments batiment
        Admin _ b -> batiment == b
        -- Eau _ -> False
        -- Route _ -> False
        _ -> True

batimentDansListe :: [Batiment] -> Batiment -> Bool
batimentDansListe [] batiment = False
batimentDansListe (b:bs) batiment =
    (batiment == b) || batimentDansListe bs batiment


-- post conditions

postcondition_ConstruitBatiment :: Ville -> Zone -> ZonId -> Batiment ->Zone -> Ville -> Bool
postcondition_ConstruitBatiment ville zone zonId batiment nouvelleZone nouvelleVille =
    zoneConstructionCorrect nouvelleZone
    &&
    not (batimentDejaPresent nouvelleVille nouvelleZone batiment)
    && 
    batimentNonPresentDansAutresZones nouvelleVille zonId nouvelleZone batiment

batimentNonPresentDansAutresZones :: Ville -> ZonId -> Zone -> Batiment -> Bool
batimentNonPresentDansAutresZones v id z b =
         all (\(zone, _) ->  not (batimentDejaPresent v z b))
        (Map.toList  (villeZones v))


-- ville = Ville {
--   villeZones = Map.fromList [(ZonId 1, zone1), (ZonId 2, zone2)],
--   villeCitoyens = Map.empty
-- }

-- zone1 = ZoneResidentielle (Rectangle (C 0 0) 10 10) []
-- zone2 = Route (Rectangle (C 10 0) 5 10)

-- batiment = Cabane {
--   forme = Rectangle (C 5 5) 3 3,
--   zoneId = ZonId 1,  -- ZoneId du bâtiment, initialement la zone cible
--   entree = C 5 8,
--   capacite = 10,
--   habitants = []
-- }

-- nouvelleVille = construireBatiment batiment zone1 (ZonId 1) ville

-- >>> precondition_ConstruitBatiment ville zone1 batiment
-- True
-- >>> postcondition_ConstruitBatiment ville zone1 (ZonId 1) batiment  zone1 nouvelleVille
-- True






