module VilleTest where

import Ville
import Zone
import Citoyen
import Types
import Forme
import Data.Map as Map
import Occupation
import Batiment


-- Création de quelques zones
zone1 = Eau (Rectangle (C 0 0) 10 5)
zone2 = Route (Rectangle (C 10 0) 5 10)
zone3 = ZoneResidentielle (Rectangle (C 0 5) 10 10) []
zone4 = ZoneIndustrielle (Rectangle (C 10 10) 10 5) []
zone5 = ZoneCommerciale (Rectangle (C 0 15) 5 10) []

-- Création de quelques citoyens
personneN1 = Personne {
  idCit = CitId 1,
  coord = C 10 10,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Etranger (Salarie (AnsResidence 1)),
  maladies = []
}

-- Vie de l'immigrant
vieN1 = Vie {
  argentEnBanque = 500,
  sante = 80,
  niveauFaim = 60,
    niveauFatigue = 40
}

-- L'immigrant n'a pas encore de vie personnelle dans la ville
viepriveeN1 = ViePersonnelle {
  maison = BatId 0, -- Pas de maison attribuée
  travail = Nothing, -- Pas de travail encore
  courses = Nothing,  -- Pas de courses prévues
    vehicules = [] -- Pas de véhicules
}

citoyen1=  Habitant personneN1 vieN1 viepriveeN1

-- citoyen2
personneN2 = Personne {
  idCit = CitId 2,
  coord = C 0 0,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Francais,
  maladies = []
}

vieN2 = Vie {
  argentEnBanque = 500,
  sante = 80,
  niveauFaim = 60,
    niveauFatigue = 40
}

viepriveeN2 = ViePersonnelle {
  maison = BatId 0, -- Pas de maison attribuée
  travail = Nothing, -- Pas de travail encore
  courses = Nothing,  -- Pas de courses prévues
    vehicules = [] -- Pas de véhicules
}

citoyen2=  Habitant personneN2 vieN2 viepriveeN2

-- citoyen 3

personneN3 = Personne {
  idCit = CitId 3,
  coord = C 0 0,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Francais,
  maladies = []
}

vieN3 = Vie {
  argentEnBanque = 500,
  sante = 80,
  niveauFaim = 60,
    niveauFatigue = 40
}

viepriveeN3 = ViePersonnelle {
  maison = BatId 0, -- Pas de maison attribuée
  travail = Nothing, -- Pas de travail encore
  courses = Nothing,  -- Pas de courses prévues
    vehicules = [] -- Pas de véhicules
}

citoyen3=  Habitant personneN3 vieN3 viepriveeN3

 -- Création de la ville
maVille = Ville {
    villeZones = Map.fromList [(ZonId 1, zone1), (ZonId 2, zone2), (ZonId 3, zone3), (ZonId 4, zone4), (ZonId 5, zone5)],
    villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
}
-- >>> show maVille
-- "Ville {\n  Zones:\n    ZonId 1: Eau Rectangle (C {cx = 0, cy = 0}) 10 5\n    ZonId 2: Route Rectangle (C {cx = 10, cy = 0}) 5 10\n    ZonId 3: Zone r\233sidentielle Rectangle (C {cx = 0, cy = 5}) 10 10 avec 0 b\226timents\n    ZonId 4: Zone industrielle Rectangle (C {cx = 10, cy = 10}) 10 5 avec 0 b\226timents\n    ZonId 5: Zone commerciale Rectangle (C {cx = 0, cy = 15}) 5 10 avec 0 b\226timents\n\n  Citoyens:\n    CitId 1: Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Etranger (Salarie (AnsResidence 1)), maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 60, niveauFatigue = 40}, Vie Personnelle : ViePersonnelle {maison = BatId 0, travail = Nothing, courses = Nothing, vehicules = []}\n    CitId 2: Habitant : Personne {idCit = CitId 2, coord = C {cx = 0, cy = 0}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 60, niveauFatigue = 40}, Vie Personnelle : ViePersonnelle {maison = BatId 0, travail = Nothing, courses = Nothing, vehicules = []}\n    CitId 3: Habitant : Personne {idCit = CitId 3, coord = C {cx = 0, cy = 0}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 60, niveauFatigue = 40}, Vie Personnelle : ViePersonnelle {maison = BatId 0, travail = Nothing, courses = Nothing, vehicules = []}\n\n}"

-- >>> :t maVille
-- maVille :: Ville


-- -- Création de zones pour la ville
zoneA = Eau (Rectangle (C 0 0) 1 1)
zoneB = Route (Rectangle (C 2 0) 1 1)
zoneC = Route (Rectangle (C 4 0) 1 1)
zoneD = Route (Rectangle (C 6 0) 1 1)
zoneE = Eau (Rectangle (C 0 2) 1 1)
zoneF = Route (Rectangle (C 2 2) 1 1)
zoneG = Eau (Rectangle (C 4 2) 1 1)
zoneH = Route (Rectangle (C 6 2) 1 1)

-- Création de la ville
ville1 = Ville {
    villeZones = Map.fromList [(ZonId 1, zoneA), (ZonId 2, zoneB), (ZonId 3, zoneC), (ZonId 4, zoneD),
                               (ZonId 5, zoneE), (ZonId 6, zoneF), (ZonId 7, zoneG), (ZonId 8, zoneH)],
    villeCitoyens = Map.empty
}

-- >>> show ville1
-- "Ville {\n  Zones:\n    ZonId 1: Eau Rectangle (C {cx = 0, cy = 0}) 1 1\n    ZonId 2: Route Rectangle (C {cx = 2, cy = 0}) 1 1\n    ZonId 3: Route Rectangle (C {cx = 4, cy = 0}) 1 1\n    ZonId 4: Route Rectangle (C {cx = 6, cy = 0}) 1 1\n    ZonId 5: Eau Rectangle (C {cx = 0, cy = 2}) 1 1\n    ZonId 6: Route Rectangle (C {cx = 2, cy = 2}) 1 1\n    ZonId 7: Eau Rectangle (C {cx = 4, cy = 2}) 1 1\n    ZonId 8: Route Rectangle (C {cx = 6, cy = 2}) 1 1\n\n  Citoyens:\n\n}"


ville = Ville {
  villeZones = Map.fromList [(ZonId 1, zoneResiden1)],
  villeCitoyens = Map.empty
}



zoneResiden1 = ZoneResidentielle (Rectangle (C 0 0) 10 10) [batiment1, batiment2]

batiment1 = Cabane {
  forme = Rectangle (C 0 0) 3 3,
  zoneId = ZonId 1,
  entree = C 0 3,
  capacite = 10,
  habitants = []
}

batiment2 = Atelier {
  forme = Rectangle (C 6 6) 4 4,
  zoneId = ZonId 1,
  entree = C 6 10,
  capacite = 15,
  Batiment.employes = []
}


-- >>> show ville
-- "Ville {\n  Zones:\n    ZonId 1: Zone r\233sidentielle Rectangle (C {cx = 0, cy = 0}) 10 10 avec 2 b\226timents\n\n  Citoyens:\n\n}"


-- test des préconditions
-- >>> preconditionSupprimerBatiment ville (ZonId 1) zoneResiden1 batiment1
-- True
-- zone1 c'est une zone d'eau
-- >>> preconditionSupprimerBatiment ville (ZonId 1) zone1 batiment1
-- False


nouvelleVille = supprimerBatiment (zoneIdBatiment batiment1) zoneResiden1 ville

-- >>> show nouvelleVille
-- "Ville {\n  Zones:\n    ZonId 1: Zone r\233sidentielle Rectangle (C {cx = 0, cy = 0}) 10 10 avec 1 b\226timents\n\n  Citoyens:\n\n}"

-- ! invariant de la ville  a rechecker


zoneInvValid = ZoneResidentielle (Rectangle (C 0 0) 10 10) [batiment1, batiment2]
zoneRouteInvValid = Route (Rectangle (C 0 0) 10 10)
villeInvValid = Ville {
  villeZones = Map.fromList [(ZonId 1, zoneInvValid), (ZonId 2, zoneRouteInvValid)],
  villeCitoyens = Map.empty
}
-- >>>  invariantVille villeInvValid
-- False


zoneAV = Eau (Rectangle (C 0 0) 2 2)
zoneBV = Route (Rectangle (C 2 2) 1 1)
zoneCV = ZoneResidentielle (Rectangle (C 4 4) 1 1) []
zoneDV = ZoneIndustrielle (Rectangle (C 6 6) 1 1) []
zoneEV = ZoneCommerciale (Rectangle (C 8 8) 1 1) []



-- Création de la ville
maVille2 = Ville {
    villeZones = Map.fromList [(ZonId 1, zoneAV), (ZonId 2, zoneBV), (ZonId 3, zoneCV), (ZonId 4, zoneDV), (ZonId 5, zoneEV)],
    villeCitoyens = Map.fromList [(CitId 1, citoyen1), (CitId 2, citoyen2), (CitId 3, citoyen3)]
}




-- >>> invariantZonesDisjointes maVille
-- False
-- >>> invariantZonesDisjointes maVille2
-- False


-- !  estAdjacenteARoute

z1 =ZoneResidentielle (Rectangle (C 0 (-2)) 2 2) []
z2= ZoneCommerciale (Rectangle (C 2 (-5)) 3 2) []
z3 =  Route (Rectangle (C 0 (-4)) 8 1)
villeTest = Ville {
    villeZones =  Map.fromList [(ZonId 1, z1), (ZonId 2, z2), (ZonId 3,z3)],
    villeCitoyens = Map.empty
}

ztestTrue= ZoneResidentielle (Rectangle (C 3 (-3)) 2 1) []
zTestFalse = ZoneResidentielle (Rectangle (C 0 (-2)) 2 2) []

-- retourne vrai
-- >>> estAdjacenteARoute ztestTrue villeTest
-- True

-- >>> estAdjacenteARoute z2 villeTest
-- True

-- >>> estAdjacenteARoute z1 villeTest
-- False

-- >>> estAdjacenteARoute zTestFalse villeTest
-- False

-- should be false
-- >>> villeVerifiantAdjacenceARoute villeTest
-- False

