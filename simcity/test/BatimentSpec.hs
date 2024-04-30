{-# OPTIONS_GHC -Wno-unused-imports #-}
module BatimentSpec where
import Batiment
import Forme
import Types
import Citoyen
-- import CitoyenSpec
import Prefecture
-- import PrefectureSpec
import Occupation -- can be deleted
import Produit
import ProduitSpec
import Vehicule
import VehiculeSpec
import Entreprise
import EntrepriseSpec
import Maladies
import Test.Hspec
import Control.Exception (evaluate)
import  Batiment



-- ***** MODIFICATIONS *****

-- des exemples
cabane :: Batiment
cabane = Cabane (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10 []
new :: Batiment
new = ajouterCapacite cabane 5
new' :: Batiment
new' = diminuerCapacite new 5
new'_ :: Batiment
new'_ = diminuerCapacite new 20
citoyen1 :: CitId
citoyen1 = CitId 1
new'' :: Batiment
new'' = ajouterCitoyen cabane citoyen1
new''' :: Batiment
new''' = retirerCitoyen new'' citoyen1


-- ****** Banque ******


-- tester les banques
banque1 :: Batiment
banque1 = Banque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 [cid1,  cid2]
banque2 :: Batiment
banque2 = Banque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 [cid1,  cid2]

banque3 :: Batiment 
banque3 = Banque (BankId 2) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 [citoyen1]

c1 :: Coord
c1 = C 1 1
cid1 :: CitId
cid1 = CitId 1
o1 :: Occupation
o1 = Travailler 100.0

crimes1 :: [a]
crimes1 =[]
nat1 :: Nationalite
nat1 = Francais
m1 :: [Maladie]
m1 =[]
p1 :: Personne
p1 = Personne cid1 c1 o1 crimes1 nat1 m1
v1 :: Vie
v1 = Vie 500 80 0 0
vp1 :: ViePersonnelle
vp1 = ViePersonnelle (BatId 1) (Just (BatId 2)) (Just (BatId 3)) []
alice :: Citoyen
alice = Habitant p1 v1 vp1 


-- alice = Habitant (C 1 1) (100, 100, 100) (BatId 1, Nothing, Nothing) Dormir

c2 :: Coord
c2 = C 2 2
cid2 :: CitId
cid2 = CitId 2
o2 :: Occupation
o2 = Travailler 100.0

crimes2 :: [a]
crimes2 =[]
nat2 :: Nationalite
nat2 = Francais
m2 :: [Maladie]
m2 =[]
p2 :: Personne
p2 = Personne cid2 c2 o2 crimes2 nat2 m2
v2 :: Vie
v2 = Vie 500 80 0 0
vp2 :: ViePersonnelle
vp2 = ViePersonnelle (BatId 1) (Just (BatId 2)) (Just (BatId 3)) []
bob :: Citoyen
bob = Habitant p2 v2 vp2

bk1 :: Batiment
bk2 :: Batiment
cit1 :: Citoyen
cit2 :: Citoyen
(bk1,bk2,cit1,cit2) = virementBancaire banque1 10 banque1 alice bob

bk1' :: Batiment
bk3 :: Batiment
cit3 :: Citoyen
cit4 :: Citoyen
(bk1',bk3,cit3,cit4) = virementBancaire banque1 10 banque3 alice bob



batimentSpec :: Spec
batimentSpec = do 
    describe "affichage d'un batiment" $ do 
        it "affiche un restaurant" $ do
            show (creerRestaurant (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Restaurant Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 clients"
        it "affiche une banque" $ do
            show (creerBanque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000) `shouldBe` "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1000.0 et 0 clients"
        it "affiche une prefecture" $ do
            show (creerPrefecture (Prefecture (PrefId 1) "Bobigny" []) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8)) `shouldBe` "Prefecture Prefecture PrefId 1 Bobigny et g\233re 0 citoyens Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1"
        it "affiche une maison" $ do
            show (creerMaison (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Maison Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"
        it "affiche une cabane" $ do
            show (creerCabane (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"
        it "affiche un atelier" $ do
            show (creerAtelier (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Atelier Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 employ\233s"
        it "affiche une epicerie" $ do
            show (creerEpicerie (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Epicerie Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 clients et StockProduit [] stock"
        it "affiche un commissariat" $ do
            show (creerCommissariat (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8)) `shouldBe` "Commissariat Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un heliport True"
        it "affiche une ecole" $ do
            show (creerEcole (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Ecole Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 \233l\232ves"
        it "affiche un hopital" $ do
            show (creerHopital (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Hopital Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 patients et un heliport True"
        it "affiche un cinema" $ do
            show (creerCinema (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10) `shouldBe` "Cinema Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 spectateurs"
    describe "modification de la capacité d'un batiment" $ do
        it "ajoute de la capacité" $ do
            show new `shouldBe` "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 15 et 0 habitants"
        it "diminue de la capacité" $ do
            show new' `shouldBe` "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"
        it "diminue de la capacité" $ do
            show new'_ `shouldBe` "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 0 et 0 habitants"
    describe "modification de la liste des citoyens d'un batiment" $ do
        it "ajoute un citoyen" $ do
            show new'' `shouldBe` "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 1 habitants"
        it "retire un citoyen" $ do
            show new''' `shouldBe` "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"
    describe "virement bancaire" $ do
        it "affichage de la première personne avant le virement" $ do
            show alice `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 1, cy = 1}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
        it "affichage de la deuxième personne avant le virement" $ do
            show bob `shouldBe` "Habitant : Personne {idCit = CitId 2, coord = C {cx = 2, cy = 2}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 500.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
        it "affichage de la première personne après le virement" $ do
            show cit1 `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 1, cy = 1}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 490.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
        it "affichage de la deuxième personne après le virement" $ do
            show cit2 `shouldBe` "Habitant : Personne {idCit = CitId 2, coord = C {cx = 2, cy = 2}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 510.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
        it "affichage de la banque 1 avant le virment" $ do
            show bk1 `shouldBe` "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1000.0 et 0 clients"
        it "affichage de la banque 2 avant le virment" $ do
            show bk2 `shouldBe` "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1000.0 et 0 clients"
        -- c'est la même banque
        it "affichage de la banque 1 après le virment" $ do
            show bk1 `shouldBe` "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1000.0 et 0 clients"
        it "affichage de la banque 2 après le virment" $ do
            show bk2 `shouldBe` "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1000.0 et 0 clients"
        describe "Nested Banks" $ do 
            it "virement entre banque 1 et 3 " $ do 
                show bk1' `shouldBe` "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 990.0 et 0 clients"
                show bk3 `shouldBe` "Banque BankId 2 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1010.0 et 1 clients"
                show cit3 `shouldBe` "Habitant : Personne {idCit = CitId 1, coord = C {cx = 1, cy = 1}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 490.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
                show cit4 `shouldBe` "Habitant : Personne {idCit = CitId 2, coord = C {cx = 2, cy = 2}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 510.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"
            describe "erreurs dans le virement " $ do 
                it "emetteur n'a pas suffisament d'argent" $ do 
                    virementCitoyenVersCitoyen alice 10000 bob  `shouldBe` Nothing
                it "la banque n'a pas suffisement d'argent" $ do
                    virementBanqueVersBanque banque1 10000 banque3 `shouldBe` Nothing
                -- it "l'emetteur n'est pas dans la banque dont il envoie de l'argent" $ do 
                --      evaluate (virementBancaire banque3 10 banque3 alice bob) `shouldThrow` errorCall "l'emetteur n'est pas un client de la banque"
                    
