module BatimentTest where
import Batiment
import Forme
import Types
import Citoyen
import Prefecture
import Occupation -- can be deleted
import Produit
import Vehicule
import Entreprise


-- ***** BASICS *****


-- >>> show (creerRestaurant (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Restaurant Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 clients"

-- >>> show (creerBanque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000)
-- "Banque BankId 1 Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un argent de 1000.0 et 0 clients"

-- >>> show (creerPrefecture (Prefecture (PrefId 1) "Bobigny" []) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8))
-- "Prefecture Prefecture PrefId 1 Bobigny et g\233re 0 citoyens Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1"

-- >>> show (creerMaison (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Maison Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"

-- >>> show (creerCabane (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"

-- >>> show (creerAtelier (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Atelier Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 employ\233s"

-- >>> show (creerEpicerie (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Epicerie Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 clients et StockProduit [] stock"


-- >>> show (creerCommissariat (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8))
-- "Commissariat Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec un heliport True"

-- >>> show (creerEcole (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Ecole Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 \233l\232ves"

-- >>> show (creerHopital (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Hopital Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 patients et un heliport True"

-- >>> show (creerCinema (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10)
-- "Cinema Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 spectateurs"



-- ***** MODIFICATIONS *****

-- des exemples
cabane = Cabane (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10 []
-- capacité est de 10
new = ajouterCapacite cabane 5
-- >>> show new
-- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 15 et 0 habitants"
new' = diminuerCapacite new 5
-- >>> show new'
-- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"
new'_ = diminuerCapacite new 20
-- >>> show new'_
-- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 0 et 0 habitants"
citoyen1 = CitId 1

new'' = ajouterCitoyen cabane citoyen1

-- >>> show new''
-- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 1 habitants"
new''' = retirerCitoyen new'' citoyen1
-- >>> show new'''
-- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"


-- ****** Banque ******


-- tester les banques
banque1 = Banque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 []
banque2 = Banque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 []

c1 = C 1 1
cid1 = CitId 1
o1 = Travailler 100.0

crimes1 =[]
nat1 = Francais
m1 =[]
p1 = Personne cid1 c1 o1 crimes1 nat1 m1
v1 = Vie 500 80 0 0
vp1 = ViePersonnelle (BatId 1) (Just (BatId 2)) (Just (BatId 3)) []
alice = Habitant p1 v1 vp1 

-- alice = Habitant (C 1 1) (100, 100, 100) (BatId 1, Nothing, Nothing) Dormir

c2 = C 2 2
cid2 = CitId 2
o2 = Travailler 100.0

crimes2 =[]
nat2 = Francais
m2 =[]
p2 = Personne cid2 c2 o2 crimes2 nat2 m2
v2 = Vie 500 80 0 0
vp2 = ViePersonnelle (BatId 1) (Just (BatId 2)) (Just (BatId 3)) []
bob = Habitant p2 v2 vp2

(bk1,bk2,cit1,cit2) = virementBancaire banque1 10 banque1 alice bob
--  >>> show cit2
-- "Habitant : Personne {idCit = CitId 2, coord = C {cx = 2, cy = 2}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais, maladies = []}, Vie : Vie {argentEnBanque = 510.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3), vehicules = []}"



--- ****** Invariants ******




--- ****** Hopital ******


hopital = Hopital (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10 [id1] True
id1 = CitId 1
personne = Personne {
  idCit = id1,
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

id2 = CitId 2
-- ! test des hopitaux 
-- >>> getCapacite hopital
-- 10

-- >>> invariantBatiment hopital
-- True

-- >>> show (ajouterPatient hopital id1)
-- le citoyen est déjà hospitalisé

-- >>> show (hospitaliser Batiment.citoyen hopital)
-- le citoyen est déjà hospitalisé

-- >>> show (sortirPatientDeL'Hopital id1 hopital)
-- "Hopital Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 patients et un heliport True"

-- >>> show (sortirPatientDeL'Hopital id2 hopital)
-- le citoyen n'est pas hospitalisé dans cette hopital



-- ****** Vente de produits ******


-- ! test des produits

produit = Produit (ProdId 1) "Pain" 0.5 (Alimentaire Pain Frais) Local

epicerie = Epicerie (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10 [] (StockProduit [(produit, Quantite 1)])

res = vendreProduit epicerie produit
-- >>> show res
-- "(Epicerie Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 clients et StockProduit [(Produit {idProd = ProdId 1, nomProd = \"Pain\", prixProd = 0.5, typeProd = Alimentaire Pain Frais, production = Local},Quantite 0)] stock,Produit {idProd = ProdId 1, nomProd = \"Pain\", prixProd = 0.5, typeProd = Alimentaire Pain Frais, production = Local})"

(b,_)=res
-- essayer de revendre le produit qui est en rupture de stock
res' = vendreProduit b produit
-- >>> show res'
-- This product is out of stock
