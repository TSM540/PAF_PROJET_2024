module CitoyenSpec where
import Citoyen
import Forme
import Occupation
import Types
import Maladies
import ProduitSpec
import Produit
import VehiculeSpec
import Test.Hspec
import Control.Exception (evaluate)



-- Données personnelles de l'immigrant
personne1 :: Personne
personne1 = Personne {
  idCit = CitId 1,
  coord = C 10 10,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Etranger (Salarie (AnsResidence 1)),
    maladies = []
}

-- Vie de l'immigrant
vie1 :: Vie
vie1 = Vie {
  argentEnBanque = 500,
  sante = 80,
  niveauFaim = 60,
    niveauFatigue = 50
}

-- L'immigrant n'a pas encore de vie personnelle dans la ville
vp1 :: ViePersonnelle
vp1 = ViePersonnelle {
  maison = BatId 0, -- Pas de maison attribuée
  travail = Nothing, -- Pas de travail encore
  courses = Nothing,  -- Pas de courses prévues
  vehicules = []
}

immigrant :: Citoyen
immigrant = Immigrant personne1 vie1

-- On attribue une vie personnelle à l'immigrant
nouvelleViePersonnelle :: ViePersonnelle
nouvelleViePersonnelle = ViePersonnelle {
  maison = BatId 1, -- On lui attribue la maison BatId 1
  travail = Just (BatId 2), -- On lui attribue le travail BatId 2
  courses = Just (BatId 3),  -- Pas de courses prévues pour le moment
    vehicules = []
}

-- On tente de transformer l'immigrant en habitant
maybeHabitant1 :: Maybe Citoyen
maybeHabitant1 = immigrantToHabitant immigrant nouvelleViePersonnelle

-- On utilise maybeValue pour extraire l'habitant si la transformation a réussi
habitant1 :: Citoyen
habitant1 = maybeValue maybeHabitant1


-- Citoyen avec des valeurs valides
citoyenValide :: Citoyen
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
citoyenSanteNegative :: Citoyen
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



-- Habitant fatigué et qui travaille
habitantFatigueTravail :: Citoyen
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
coordMaison :: Coord
coordMaison = C 1 1

-- Test de la fonction
maybeHabitant :: Maybe Citoyen
maybeHabitant = revenirMaison habitantFatigueTravail coordMaison
val :: Citoyen
val = maybeValue maybeHabitant


-- Habitant fatigué
habitantFatigue :: Citoyen
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
nbHeuresSommeil :: Int
nbHeuresSommeil = 8



-- Test de la fonction
habitant :: Citoyen
habitant = dormir (Just habitantFatigue) nbHeuresSommeil



--- Test des fonctions
personne :: Personne
personne = Personne {
  idCit = CitId 1,
  coord = C 0 0,
  occupation = Travailler 100.0,
  crimes = [],
  nationalite = Francais,
  maladies = []
}
v :: Vie
v = Vie {
  argentEnBanque = 1000,
  sante = 100,
  niveauFaim = 50,
  niveauFatigue = 50
}
vp :: ViePersonnelle
vp = ViePersonnelle {
  maison = BatId 1,
  travail = Just (BatId 2),
  courses = Just (BatId 3),
  vehicules = []
} 

citoyen :: Citoyen
citoyen = Habitant personne v vp
destination :: Coord
destination = C 10 10

maladie :: Maladie
maladie = Maladie {
  nomMaladie = "Grippe",
  symptomes = ["Fièvre", "Toux"],
  traitement = "Repos",
  typeMaladie = Infectieuse

}

produit :: Produit
produit = Produit {
  idProd = ProdId 1,
  nomProd = "Pain",
  prixProd = 1.0,
  typeProd = Alimentaire Pain Frais,
  production = Local
}



citoyenSpec :: Spec 
citoyenSpec = do 
    describe "affichage de l'habitant" $ do 
        it "immigrant vers un habitant " $ do 
             habitant1 `shouldBe` Habitant (Personne {idCit = CitId 1, coord = C 10 10, occupation = Travailler 100.0, crimes = [], nationalite = Etranger (Salarie (AnsResidence 1)), maladies = []}) (Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 60, niveauFatigue = 50}) (ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []})
    describe "invariant de citoyen" $ do 
        it "citoyen valide" $ do 
            citoyenValide `shouldSatisfy` invCitoyen 
        it "citoyen avec une santé négative" $ do 
            citoyenSanteNegative `shouldNotSatisfy` invCitoyen
    describe "retour à la maison" $ do

        it "habitant non fatigué" $ do
            revenirMaison habitant1 coordMaison `shouldBe` Nothing
        it "habitant non fatigué et qui ne travaille pas" $ do
            revenirMaison habitant1 coordMaison `shouldBe` Nothing
        it "habitant non fatigué" $ do
            revenirMaison habitant1 coordMaison `shouldBe` Nothing
        it "habitant non fatigué et qui ne travaille pas" $ do
            revenirMaison habitant1 coordMaison `shouldBe` Nothing
    describe "dormir " $ do 
        it "habitant fatigué" $ do 
           show  habitant `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Dormir 8 heures, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 100, niveauFaim = 60, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
    describe "se reveiller" $ do 
        it "citoyen qui se reveil " $ do
            show ( seReveiller citoyen) `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Rester \224 la maison, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
    describe "se deplacer "$ do 
        it "citoyen qui se deplace" $ do 
            show (seDeplacer citoyen destination) `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 10, cy = 10}, occupation = Se d\233placer vers C {cx = 10, cy = 10}, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
    describe "manger un produit" $ do 
        it "citoyen qui mange un produit" $ do 
            show (manger citoyen produit) `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Manger, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 0, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
    describe "cuisiner un produit" $ do
        it "citoyen qui cuisine un produit" $ do 
            show (cuisiner citoyen produit) `shouldBe` "(Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Cuisiner, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 55}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []},Produit {idProd = ProdId 1, nomProd = \"Pain\", prixProd = 1.0, typeProd = Alimentaire Pain Cuit, production = Local})"
    describe "tomber malade " $ do 
        it " citoyen qui tombe malade " $ do 
            show (tomberMalade citoyen maladie) `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = [Maladie {nomMaladie = \"Grippe\", symptomes = [\"Fi\\232vre\",\"Toux\"], traitement = \"Repos\", typeMaladie = Infectieuse}]}, Vie : Vie {argentEnBanque = 1000.0, sante = 90, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
    describe "guerir d'une maladie" $ do 
        it "citoyen qui guerit d'une maladie" $ do 
            show (guerirD'uneMaladie citoyen maladie) `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 0, cy = 0}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 1000.0, sante = 100, niveauFaim = 50, niveauFatigue = 50}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
    describe "acheter un vehicule" $ do 
        it "achat d'un vehicule " $ do
            show (snd (acheterVehiculeParCitoyen citoyen vehic4)) `shouldBe` "Vehicule {idVehic = VehicId 4, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeCitoyen (CitId 1)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}"
