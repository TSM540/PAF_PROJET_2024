{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module CitoyenTest where
import Citoyen
import Forme
import Occupation
import Types
import Maladies
import Produit
import Vehicule 
import VehiculeTest


-- Données personnelles de l'immigrant
personne1 = Personne {
  idCit = CitId 1,
  coord = C 10 10,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Etranger (Salarie (AnsResidence 1)),
    maladies = []
}

-- Vie de l'immigrant
vie1 = Vie {
  argentEnBanque = 500,
  sante = 80,
  niveauFaim = 60,
    niveauFatigue = 50
}

-- L'immigrant n'a pas encore de vie personnelle dans la ville
vp1 = ViePersonnelle {
  maison = BatId 0, -- Pas de maison attribuée
  travail = Nothing, -- Pas de travail encore
  courses = Nothing,  -- Pas de courses prévues
  vehicules = []
}

immigrant = Immigrant personne1 vie1

-- On attribue une vie personnelle à l'immigrant
nouvelleViePersonnelle = ViePersonnelle {
  maison = BatId 1, -- On lui attribue la maison BatId 1
  travail = Just (BatId 2), -- On lui attribue le travail BatId 2
  courses = Just (BatId 3),  -- Pas de courses prévues pour le moment
    vehicules = []
}

-- On tente de transformer l'immigrant en habitant
maybeHabitant1 = immigrantToHabitant immigrant nouvelleViePersonnelle

-- On utilise maybeValue pour extraire l'habitant si la transformation a réussi
habitant1 = maybeValue maybeHabitant1

-- Affichage de l'habitant (si la transformation a réussi)

-- >>>   show (immigrantToHabitant immigrant vp)
-- "Just Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Etranger (Salarie (AnsResidence 1)), maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 60, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"

-- >>> show habitant
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Dormir 8 heures, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 100, niveauFaim = 60, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"

-- *****************************************************************

-- Citoyen avec des valeurs valides
citoyenValide = Habitant Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Etranger (Etudiant (AnsSejour 2) Obtenu),
      maladies = []
      }
    Vie
      { argentEnBanque = 500.0
      , sante = 80
      , niveauFaim = 60
      , niveauFatigue = 40
      }
   ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3),
      vehicules = []
      }


-- Citoyen avec une santé négative
citoyenSanteNegative = Habitant
  Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Francais,
      maladies = []
      }
  Vie
      { argentEnBanque = 500.0
      , sante = -10
      , niveauFaim = 60
      , niveauFatigue = 40
      }
   ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3),
      vehicules = []
      }




-- Test de l'invariant

-- >>> invCitoyen citoyenValide
-- True
-- >>> invCitoyen citoyenSanteNegative
-- False

-- Habitant fatigué et qui travaille
habitantFatigueTravail = Habitant
  Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Francais,
      maladies = []
      }
  Vie
      { argentEnBanque = 500.0
      , sante = 80
      , niveauFaim = 60
      , niveauFatigue = 95
      }
  ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3),
        vehicules = []
      }

-- Coordonnées de la maison
coordMaison = C 1 1

-- Test de la fonction
maybeHabitant = revenirMaison habitantFatigueTravail coordMaison
val = maybeValue maybeHabitant
-- >>> show val
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Se d\233placer vers C {cx = 1, cy = 1}, crimes = [], nationalite = Francais}, Vie : Vie {argentEnBanque = 600.0, sante = 80, niveauFaim = 0, niveauFatigue = 90}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3)}"

-- Habitant fatigué
habitantFatigue = Habitant
  Personne
      { idCit = CitId 1
      , coord = C 10 10
      , occupation = Travailler 100.0
      , crimes = []
      , nationalite = Francais,
      maladies = []
      }
   Vie
      { argentEnBanque = 500.0
      , sante = 80
      , niveauFaim = 60
      , niveauFatigue = 95
      }
  ViePersonnelle
      { maison = BatId 1
      , travail = Just (BatId 2)
      , courses = Just (BatId 3),
      vehicules = []
      }




-- ! Nombre d'heures de sommeil
nbHeuresSommeil = 8

-- Test de la fonction
habitant = dormir (Just habitantFatigue) nbHeuresSommeil

-- Affichage de l'habitant
-- >>>  habitant
-- Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Dormir 8 heures, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 100, niveauFaim = 60, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}


-- *****************************************************************



--- Test des fonctions
personne = Personne {
  idCit = CitId 1,
  coord = C 0 0,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Francais,
  maladies = []
}
v = Vie {
  argentEnBanque = 1000,
  sante = 100,
  niveauFaim = 50,
  niveauFatigue = 50
}
vp = ViePersonnelle {
  maison = BatId 1,
  travail = Just (BatId 2),
  courses = Just (BatId 3),
  vehicules = []
} 

citoyen = Habitant personne v vp
destination = C 10 10

maladie = Maladie {
  nomMaladie = "Grippe",
  symptomes = ["Fièvre", "Toux"],
  traitement = "Repos",
  typeMaladie = Infectieuse

}

produit = Produit {
  idProd = ProdId 1,
  nomProd = "Pain",
  prixProd = 1.0,
  typeProd = Alimentaire Pain Frais,
  production = Local
}




-- >>> show $ seReveiller citoyen
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Rester \224 la maison, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
 
-- >>> show $ seDeplacer citoyen destination
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Se d\233placer vers C {cx = 10, cy = 10}, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"

-- >>> show $ manger citoyen produit
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Manger, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 0, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"


-- >>> :t cuisiner
-- cuisiner :: Citoyen -> Produit -> (Citoyen, Produit)

-- >>> show  ( cuisiner citoyen produit)
-- "(Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Cuisiner, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 55}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []},Produit {idProd = ProdId 1, nomProd = \"Pain\", prixProd = 1.0, typeProd = Alimentaire Pain Cuit, production = Local})"


-- >>> show $ tomberMalade citoyen maladie
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = [Maladie {nomMaladie = \"Grippe\", symptomes = [\"Fi\\232vre\",\"Toux\"], traitement = \"Repos\", typeMaladie = Infectieuse}]}, Vie : Vie {argentEnBanque = 1000.0, sante = 90, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"


-- >>> show $ guerirD'uneMaladie citoyen maladie
-- "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"


-- ***************************************************************** Vehicules



-- >>> acheterVehiculeParCitoyen citoyen vehic1
-- Ce vehicule appartient a une entreprise

-- >>> acheterVehiculeParCitoyen citoyen vehic2
-- Ce citoyen est deja proprietaire de ce vehicule

-- >>> acheterVehiculeParCitoyen citoyen vehic3
-- Ce vehicule appartient a une entreprise

-- >>> show $ snd (acheterVehiculeParCitoyen citoyen vehic4)
-- "Vehicule {idVehic = VehicId 4, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeCitoyen (CitId 1)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}"
