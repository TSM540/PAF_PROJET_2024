{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ville where

import Zone
import Citoyen
import Types
import Forme
import Data.Map as Map
import Occupation
import Batiment
import Vehicule
import Parking

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


-- invariant 
invariantVille :: Ville -> Bool
invariantVille  v=
            invariantZonesDisjointes v
             &&
            villeVerifiantAdjacenceARoute v
            &&
            routesConnexes v
            &&
            invariantBatiments v

--  toutes les zones soit disjointes deux `a deux
-- on écrit une fonction zonesDisjointes de Zone -> Zone -> Bool pour vérifier que deux zones sont disjointes 
-- on rappele que deux zones sont disjointes si elles n'ont pas de collision
--et puis  on utilise cette  fonction(zonesDisjointes) et on map sur toutes les zones de la ville pour vérifier que toutes les zones sont disjointes deux à deux 


invariantZonesDisjointes :: Ville -> Bool
invariantZonesDisjointes ville =
    all (\(zonId1, zone1) -> all (\(zonId2, zone2) -> zonId1 == zonId2
    || zonesDisjointes zone1 zone2) (Map.toList (villeZones ville))) (Map.toList (villeZones ville))


-- Fonction pour vérifier si une zone est adjacente à au moins une zone de route
-- estAdjacenteARoute :: Zone -> Ville -> Bool
-- estAdjacenteARoute zone ville =
--     any (\(_, z) -> case z of
--                       Route _ -> True
--                       _       -> False) (Map.toList (villeZones ville))
estAdjacenteARoute :: Zone -> Ville -> Bool
estAdjacenteARoute z ville = any (\z' -> case z' of
        Route _ -> adjacentes (zoneForme z) (zoneForme z');
        _ -> False) (Map.elems (villeZones ville))


-- Fonction pour vérifier si toutes les zones vérifient l'adjacence à une zone de route
villeVerifiantAdjacenceARoute :: Ville -> Bool
villeVerifiantAdjacenceARoute ville =
    all (\(_, z) -> case z of
                      Route _ -> True -- Les zones de route sont toujours adjacentes à une zone de route
                      _       -> estAdjacenteARoute z ville) (Map.toList (villeZones ville))


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



-- : invariant des batiments

invariantBatiments :: Ville -> Bool
invariantBatiments v =
    all (\(zid, z) -> case z of
                        ZoneResidentielle _ batiments -> all (batimentDansZone v zid) batiments
                        ZoneIndustrielle _ batiments -> all (batimentDansZone v zid) batiments
                        ZoneCommerciale _ batiments -> all (batimentDansZone v zid) batiments
                        Admin _ b -> batimentDansZone v zid b
                        _ -> True) (Map.toList (villeZones v))

batimentDansZone :: Ville -> ZonId -> Batiment -> Bool
batimentDansZone v zid b =
    case b of
        Cabane _ zid' _ _ _ -> zid == zid'
        Atelier _ zid' _ _ _ -> zid == zid'
        Epicerie _ zid' _ _ _  _ -> zid == zid'
        Commissariat _ zid' _ _ -> zid == zid'

-- ! Suppresion d'un batiment

supprimerBatiment :: ZonId -> Zone -> Ville -> Ville
supprimerBatiment zonId zone ville =
    let nouvelleZone = case zone of
                        ZoneResidentielle _ batiments -> ZoneResidentielle (zoneForme zone) (supprimerBatimentDansListe batiments zonId)
                        ZoneIndustrielle _ batiments -> ZoneIndustrielle (zoneForme zone) (supprimerBatimentDansListe batiments zonId)
                        ZoneCommerciale _ batiments -> ZoneCommerciale (zoneForme zone) (supprimerBatimentDansListe batiments zonId)
                        Admin _ batimentUnique -> Admin (zoneForme zone) batimentUnique
                        _ -> zone
        zonesMisesAJour = remplacerZone zonId nouvelleZone (villeZones ville)
    in ville { villeZones = zonesMisesAJour }

supprimerBatimentDansListe :: [Batiment] -> ZonId -> [Batiment]
supprimerBatimentDansListe [] _ = []
supprimerBatimentDansListe (b:bs) zonId =
    if zonId == zoneIdBatiment b then bs else b : supprimerBatimentDansListe bs zonId

zoneIdBatiment :: Batiment -> ZonId
zoneIdBatiment batiment = case batiment of
  Cabane {zoneId = zid} -> zid
  Atelier {zoneId = zid} -> zid
  Epicerie {zoneId = zid} -> zid
  Commissariat {zoneId = zid} -> zid
  _ -> error "Unexpected building type"

preconditionSupprimerBatiment :: Ville -> ZonId -> Zone -> Batiment -> Bool
preconditionSupprimerBatiment ville zonId z batiment =
  batimentDansZone ville zonId batiment
  &&
  case z of
    ZoneResidentielle _ _ -> True
    ZoneIndustrielle _ _ -> True
    ZoneCommerciale _ _ -> True
    _ -> False

postconditionSupprimerBatiment :: Ville -> ZonId -> Zone -> Batiment -> Ville -> Bool
postconditionSupprimerBatiment ville zonId zone batiment nouvelleVille =
--   batimentDansZone nouvelleVille zonId batiment
    batimentDansZone nouvelleVille zonId batiment


-- faire rouler les vehicules

roulerVehicule :: Vehicule -> Ville -> Zone -> Zone -> Parking-> Parking -> (Parking,Parking)
roulerVehicule vehicule v zoneDepart zoneArrivee parkingDepart parkingArrivee
        | zoneDepart == zoneArrivee = error "Le vehicule est deja dans la zone d'arrivee"
        | not (estZoneRoutiere zoneDepart) = error "La zone de depart n'est pas une zone routiere"
        | not (estZoneRoutiere zoneArrivee) = error "La zone d'arrivee n'est pas une zone routiere"
        -- | not (routesConnexes v) = error "Les zones de depart et d'arrivee ne sont pas connectees"
        | not (roulerCorrectement vehicule) = error "Le vehicule ne peut pas rouler"
        | otherwise =
            let resultatEnlevement = enleverVoitureDuParking (getVehiculeId vehicule) parkingDepart in
                case resultatEnlevement  of
                    parkingEnlevee ->
                        let resultatAjout = ajouterVoitureAuParking (getVehiculeId vehicule) parkingArrivee in
                            case resultatAjout of
                                Just parkingAjoutee -> (parkingEnlevee, parkingAjoutee)
                                Nothing -> error "Le parking d'arrivee est plein"
                    _ -> error "La voiture n'est pas dans le parking de depart"


-- Gestion des héliports et des hélicoptères

-- | Atterrir un hélicoptère sur un héliport
atterirHelico :: Vehicule -> Batiment-> Batiment 
atterirHelico vehicule batiment 
            | getTypeVehicle vehicule == Helicoptere =
                case batiment of
                    CasernePompier _ _ _ _ _ h -> if h then batiment {heliport = False} else error "L'héliport n'est pas disponible, vous ne pouvez pas atterir maintenant"
                    Hopital _ _ _ _ _ h -> if h then batiment {heliport = False} else error "L'héliport n'est pas disponible, vous ne pouvez pas atterir maintenant" 
                    _ -> error "Le batiment n'a pas un héliport"
            | otherwise = error "Le vehicule n'est pas un hélicoptère"
                        
decollerHelico :: Vehicule -> Batiment -> Batiment
decollerHelico vehicule batiment 
            | getTypeVehicle vehicule == Helicoptere =
                case batiment of
                    CasernePompier _ _ _ _ _ h -> if not h then batiment {heliport = True} else error "Il n'y a pas d'hélicoptère à la caserne car l'héliport est disponible"
                    Hopital _ _ _ _ _ h -> if not h then batiment {heliport = True} else error "Il n'y a pas d'hélicoptère à l'hôpital car l'héliport est disponible" 
                    _ -> error "Le batiment n'a pas un héliport"
            | otherwise = error "Le vehicule n'est pas un hélicoptère"