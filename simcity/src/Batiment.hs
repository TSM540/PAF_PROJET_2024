module Batiment where
import Forme
import Types
import Citoyen
import Prefecture
import Occupation -- can be deleted
data Batiment = Cabane {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de citoyens que cette cabane peut contenir
                    habitants :: [CitId]  -- Liste des citoyens habitant cette cabane
                }
              | Atelier {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de citoyens que cet atelier peut contenir
                    employes :: [CitId]  -- Liste des citoyens employés dans cet atelier
                }
              | Epicerie {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de clients que cette épicerie peut contenir
                    clients :: [CitId]  -- Liste des citoyens clients de cette épicerie
                }
              | Commissariat {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord
                }
              | Ecole {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale d'élèves que cette école peut contenir
                    eleves :: [CitId]  -- Liste des citoyens élèves de cette école
                }
              | Hopital {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de patients que cet hôpital peut contenir
                    patients :: [CitId]  -- Liste des citoyens patients de cet hôpital
                }
              | Cinema {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de spectateurs que ce cinéma peut contenir
                    spectateurs :: [CitId]  -- Liste des citoyens spectateurs de ce cinéma
                }
              | Restaurant {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de clients que ce restaurant peut contenir
                    clients :: [CitId]  -- Liste des citoyens clients de ce restaurant
                }
              | Banque {
                    idBanque :: BankId,
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    argent :: Float,  -- Argent total de cette banque
                    clients :: [CitId]  -- Liste des citoyens clients de cette banque
                }
              | Pref {
                    pref :: Prefecture,
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord
                }

                deriving(Eq)


-- instances

instance Show Batiment where
    show (Cabane forme zoneId _ capacite habitants) = "Cabane " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length habitants) ++ " habitants"
    show (Atelier forme zoneId _ capacite employes) = "Atelier " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length employes) ++ " employés"
    show (Epicerie forme zoneId _ capacite clients) = "Epicerie " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length clients) ++ " clients"
    show (Commissariat forme zoneId _) = "Commissariat " ++ show forme ++ " dans la zone " ++ show zoneId
    show (Ecole forme zoneId _ capacite eleves) = "Ecole " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length eleves) ++ " élèves"
    show (Hopital forme zoneId _ capacite patients) = "Hopital " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length patients) ++ " patients"
    show (Cinema forme zoneId _ capacite spectateurs) = "Cinema " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length spectateurs) ++ " spectateurs"
    show (Restaurant forme zoneId _ capacite clients) = "Restaurant " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length clients) ++ " clients"
    show (Banque idBanque forme zoneId _ argent clients) = "Banque " ++ show idBanque ++ " " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec un argent de " ++ show argent ++ " et " ++ show (length clients) ++ " clients"
    show (Pref pref forme zoneId _) = "Prefecture " ++ show pref ++ " " ++ show forme ++ " dans la zone " ++ show zoneId

    
-- fonctions pour gérer notre batiment
-- on a supposé que les seul modifications possibles sont les cas dessus car c'est plutot pas trop réel de changer le type d'un batiment ou son entrée
-- voir qu'il y a des cas dans la vraie vie où on peut changer le type d'un batiment mais ce n'est pas un généralité
ajouterCapacite :: Batiment -> Int -> Batiment
ajouterCapacite (Cabane forme zoneId entree capacite habitants) n = Cabane forme zoneId entree (capacite + n) habitants
ajouterCapacite (Atelier forme zoneId entree capacite employes) n = Atelier forme zoneId entree (capacite + n) employes
ajouterCapacite (Epicerie forme zoneId entree capacite clients) n = Epicerie forme zoneId entree (capacite + n) clients
ajouterCapacite (Commissariat forme zoneId entree) _ = Commissariat forme zoneId entree
ajouterCapacite (Ecole forme zoneId entree capacite eleves) n = Ecole forme zoneId entree (capacite + n) eleves
ajouterCapacite (Hopital forme zoneId entree capacite patients) n = Hopital forme zoneId entree (capacite + n) patients
ajouterCapacite (Cinema forme zoneId entree capacite spectateurs) n = Cinema forme zoneId entree (capacite + n) spectateurs
ajouterCapacite (Restaurant forme zoneId entree capacite clients) n = Restaurant forme zoneId entree (capacite + n) clients

diminuerCapacite :: Batiment -> Int -> Batiment
diminuerCapacite (Cabane forme zoneId entree capacite habitants) n = Cabane forme zoneId entree (max (capacite - n) 0) habitants
  --                                                                                        le max c'est juste pour ne pas avoir des valeurs négatives
diminuerCapacite (Atelier forme zoneId entree capacite employes) n = Atelier forme zoneId entree (max (capacite - n) 0) employes
diminuerCapacite (Epicerie forme zoneId entree capacite clients) n = Epicerie forme zoneId entree (max (capacite - n) 0) clients
diminuerCapacite (Commissariat forme zoneId entree) _ = Commissariat forme zoneId entree
diminuerCapacite (Ecole forme zoneId entree capacite eleves) n = Ecole forme zoneId entree (max (capacite - n) 0) eleves
diminuerCapacite (Hopital forme zoneId entree capacite patients) n = Hopital forme zoneId entree (max (capacite - n) 0) patients
diminuerCapacite (Cinema forme zoneId entree capacite spectateurs) n = Cinema forme zoneId entree (max (capacite - n) 0) spectateurs
diminuerCapacite (Restaurant forme zoneId entree capacite clients) n = Restaurant forme zoneId entree (max (capacite - n) 0) clients

ajouterCitoyen :: Batiment -> CitId -> Batiment
ajouterCitoyen (Cabane forme zoneId entree capacite habitants) citId = Cabane forme zoneId entree capacite (citId:habitants)
ajouterCitoyen (Atelier forme zoneId entree capacite employes) citId = Atelier forme zoneId entree capacite (citId:employes)
ajouterCitoyen (Epicerie forme zoneId entree capacite clients) citId = Epicerie forme zoneId entree capacite (citId:clients)
ajouterCitoyen (Commissariat forme zoneId entree) _ = Commissariat forme zoneId entree

retirerCitoyen :: Batiment -> CitId -> Batiment
retirerCitoyen (Cabane forme zoneId entree capacite habitants) citId = Cabane forme zoneId entree capacite (filter (/= citId) habitants)
retirerCitoyen (Atelier forme zoneId entree capacite employes) citId = Atelier forme zoneId entree capacite (filter (/= citId) employes)
retirerCitoyen (Epicerie forme zoneId entree capacite clients) citId = Epicerie forme zoneId entree capacite (filter (/= citId) clients)
retirerCitoyen (Commissariat forme zoneId entree) _ = Commissariat forme zoneId entree


-- -- des exemples
-- cabane = Cabane (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 10 []
-- -- capacité est de 10
-- new = ajouterCapacite cabane 5
-- -- >>> show new
-- -- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 15 et 0 habitants"
-- new' = diminuerCapacite new 5
-- -- >>> show new'
-- -- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"
-- new'_ = diminuerCapacite new 20
-- -- >>> show new'_
-- -- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 0 et 0 habitants"
-- citoyen1 = CitId 1

-- new'' = ajouterCitoyen cabane citoyen1

-- -- >>> show new''
-- -- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 1 habitants"
-- new''' = retirerCitoyen new'' citoyen1
-- -- >>> show new'''
-- -- "Cabane Rectangle (C {cx = 0, cy = 0}) 10 10 dans la zone ZonId 1 avec une capacit\233 de 10 et 0 habitants"



virementBancaire :: Batiment -> Float -> Batiment->Citoyen-> Citoyen-> (Batiment,Batiment, Citoyen, Citoyen)
virementBancaire b1 montant b2 emetteur recepteur = 
                  let b1'= getFstMaybe(virementBanqueVersBanque b1 montant b2) in
                  let b2' = getSndMaybe(virementBanqueVersBanque b1 montant b2) in
                  let emetteur' = getFstMaybe(virementCitoyenVersCitoyen emetteur montant recepteur) in
                  let recepteur' = getSndMaybe(virementCitoyenVersCitoyen emetteur montant recepteur) in
                   if b1 == b2 then (b1,b1,emetteur',recepteur') -- si je suis dans la même banque je ne change rien dans le montant de la banque, je change juste le montant des des deux comptes
                   else  (b1', b2', emetteur', recepteur')
                  


virementBanqueVersBanque :: Batiment -> Float -> Batiment -> Maybe (Batiment, Batiment)
virementBanqueVersBanque (Banque id forme zoneId entree argent clients) montant (Banque id' forme' zoneId' entree' argent' clients') =
        if argent - montant < 0 then Nothing
        else Just (Banque id forme zoneId entree (argent - montant) clients, Banque id' forme' zoneId' entree' (argent' + montant) clients')

virementCitoyenVersCitoyen :: Citoyen -> Float -> Citoyen-> Maybe (Citoyen,Citoyen)
virementCitoyenVersCitoyen (Habitant (Personne id c o cri nat) (Vie argent sante nivFaim nivFatigue) vp) 
                    montant 
                  (Habitant (Personne id' c' o' cri' nat') (Vie argent' sante' nivFaim' nivFatigue') vp') = 
                    if argent < montant then Nothing 
                     else Just (Habitant (Personne id c o cri nat) (Vie (argent - montant) sante nivFaim nivFatigue) vp, Habitant (Personne id' c' o' cri' nat') (Vie (argent' + montant) sante' nivFaim' nivFatigue') vp')

-- recuperer les valeurs du maybe
getFstMaybe :: Maybe (a,a) -> a
getFstMaybe (Just (first,_)) = first
getFstMaybe Nothing = error "Nothing to get"

getSndMaybe :: Maybe (a,b) -> b
getSndMaybe (Just (_,b)) = b
getSndMaybe Nothing = error "Nothing to get"



banque1 = Banque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 []
banque2 = Banque (BankId 1) (Rectangle (C 0 0) 10 10) (ZonId 1) (C 5 8) 1000 []

c1 = C 1 1
cid1 = CitId 1
o1 = Travailler 100.0

crimes1 =[]
nat1 = Francais
p1 = Personne cid1 c1 o1 crimes1 nat1
v1 = Vie 500 80 0 0
vp1 = ViePersonnelle (BatId 1) (Just (BatId 2)) (Just (BatId 3))
alice = Habitant p1 v1 vp1

-- alice = Habitant (C 1 1) (100, 100, 100) (BatId 1, Nothing, Nothing) Dormir

c2 = C 2 2
cid2 = CitId 2
o2 = Travailler 100.0

crimes2 =[]
nat2 = Francais
p2 = Personne cid2 c2 o2 crimes2 nat2
v2 = Vie 500 80 0 0
vp2 = ViePersonnelle (BatId 1) (Just (BatId 2)) (Just (BatId 3))
bob = Habitant p2 v2 vp2

(bk1,bk2,cit1,cit2) = virementBancaire banque1 10 banque1 alice bob
--  >>> show cit2
-- "Habitant : Personne {idCit = CitId 2, coord = C {cx = 2, cy = 2}, occupation = Travailler avec 100.0\8364 de salaire journalier, crimes = [], nationalite = Francais}, Vie : Vie {argentEnBanque = 510.0, sante = 80, niveauFaim = 0, niveauFatigue = 0}, Vie Personnelle : ViePersonnelle {maison = BatId 1, travail = Just (BatId 2), courses = Just (BatId 3)}"
