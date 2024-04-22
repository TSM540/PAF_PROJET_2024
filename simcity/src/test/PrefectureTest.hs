module PrefectureTest where

import Prefecture
import Citoyen
import Types
import Forme
import Occupation


c1= Habitant
          ( Personne
              { 
                idCit = CitId 1
              , coord = C 10 10
              , occupation = Travailler 100.0
              , crimes = []
              , nationalite = Etranger (Salarie (AnsResidence 5)),
                maladies = []
              }
          )
          ( Vie
              { argentEnBanque = 500
              , sante = 80
              , niveauFaim = 60,
                niveauFatigue = 0
              }
          )
          ( ViePersonnelle
              { maison = BatId 1
              , travail = Just (BatId 2)
              , courses = Just (BatId 3),
                vehicules = [VehicId 1]
              }
          )
c2= Habitant
          ( Personne
              { 
                idCit = CitId 2
              , coord = C 10 10
              , occupation = Travailler 100.0
              , crimes = []
              , nationalite = Etranger (Etudiant (AnsSejour 2 ) Obtenu),
              maladies = []
              }
          )
          ( Vie
              { argentEnBanque = 500
              , sante = 80
              , niveauFaim = 60,
                niveauFatigue = 0
              }
          )
          ( ViePersonnelle
              { maison = BatId 1
              , travail = Just (BatId 2)
              , courses = Just (BatId 3),
                vehicules = [VehicId 2]
              }
          )
c3= Habitant
          ( Personne
              { 
                idCit = CitId 3
              , coord = C 10 10
              , occupation = Travailler 100.0
              , crimes = []
              , nationalite = Francais,
              maladies = []
              }
          )
          ( Vie
              { argentEnBanque = 500
              , sante = 80
              , niveauFaim = 60,
                niveauFatigue = 0
              }
          )
          ( ViePersonnelle
              { maison = BatId 1
              , travail = Just (BatId 2)
              , courses = Just (BatId 3),
                vehicules = [VehicId 3]
              }
          )
prefecture = Prefecture
  { idPrefecture = PrefId 1
  , nom = "Paris"
  , citoyens =
      [ 
        c1,c2,c3
      ]
  }

-- Naturalisation de l'habitant
np1 = naturalisation prefecture c1

-- >>> show np1
-- "Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}"
np2 = naturalisation prefecture c2
-- >>> show np2
-- "Personne {idCit = CitId 2, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}"
np3 = naturalisation prefecture c3

-- >>> show np3
-- Vous êtes déjà français
