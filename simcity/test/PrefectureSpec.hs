module PrefectureSpec where

import Prefecture
import Citoyen
import Types
import Forme
import Occupation
import Test.Hspec
import Control.Exception (evaluate)


c1 :: Citoyen
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
c2 :: Citoyen
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
c3 :: Citoyen
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
c4 :: Citoyen
c4= Habitant
          ( Personne
              { 
                idCit = CitId 4
              , coord = C 10 10
              , occupation = Travailler 100.0
              , crimes = ["mon crime"]
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
c5:: Citoyen
c5 = Habitant
                ( Personne
                    { 
                      idCit = CitId 5
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

c6 :: Citoyen
c6= Habitant
          ( Personne
              { 
                idCit = CitId 6
              , coord = C 10 10
              , occupation = Travailler 100.0
              , crimes = []
              , nationalite = Etranger (Etudiant (AnsSejour 2 ) (EnCours (AnneesEtudesPrevu 3)) ),
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
c7 :: Citoyen
c7 = Immigrant ( Personne {
                  idCit = CitId 7
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
c8 :: Citoyen
c8 = Emigrant ( Personne {
                  idCit = CitId 8
                , coord = C 10 10
                , occupation = Travailler 100.0
                , crimes = []
                , nationalite = Etranger (Salarie (AnsResidence 5)),
                  maladies = []
                }
                )

-- un salarié avec moins de 5 ans de résidence 
c9 :: Citoyen
c9 = Habitant
          ( Personne
              { 
                idCit = CitId 9
              , coord = C 10 10
              , occupation = Travailler 100.0
              , crimes = []
              , nationalite = Etranger (Salarie (AnsResidence 4)),
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
              

prefecture :: Prefecture
prefecture = Prefecture
  { idPrefecture = PrefId 1
  , nom = "Paris"
  , citoyens =
      [ 
        c1,c2,c3,c4,c6,c7,c8,c9
      ]
  }


-- Naturalisation de l'habitant
np1 :: Personne
np1 = naturalisation prefecture c1
np2 :: Personne
np2 = naturalisation prefecture c2

prefectureSpec :: Spec
prefectureSpec = do 
    describe "Naturalisation" $ do 
        it "naturalisation de l'habitant" $ do
            show np1 `shouldBe` 
                "Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}"
        it "naturalisation de l'habitant" $ do
            show np2 `shouldBe` 
                "Personne {idCit = CitId 2, coord = C {cx = 10, cy = 10}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}"
        it "naturalisation de l'habitant français " $ do
            evaluate (naturalisation prefecture c3) `shouldThrow` errorCall "Vous êtes déjà français"
        it "naturalisation de l'habitant" $ do
            evaluate (naturalisation prefecture c4 ) `shouldThrow` errorCall "Vous avez des crimes, vous ne pouvez pas être naturalisé"
        it "naturalisation de l'habitant pas dans la prefecture" $ do
            evaluate (naturalisation prefecture c5) `shouldThrow` errorCall " Vous n'êtes dépendant de cette préfecture"
        it "d'un étudiant sans diplome obtenu" $ do
            evaluate (naturalisation prefecture c6) `shouldThrow` errorCall " diplome non obtenu ou ansSejour < 2" 
        it "d'un immigrant" $ do 
            evaluate (naturalisation prefecture c7) `shouldThrow` errorCall "Vous êtes Immigrant, vous ne pouvez pas être naturalisé car vous ne dispôsez pas des ressources nécessaires (Habitat, Travail)"
        it "d'un émigré" $ do
            evaluate (naturalisation prefecture c8) `shouldThrow` errorCall "Vous êtes bien émigré, vous ne pouvez pas être naturalisé "
        it "d'un salarié avec moins de 5 ans de résidence" $ do
            evaluate (naturalisation prefecture c9) `shouldThrow` errorCall " vous n'avez pas atteint 5 ans de résidence "