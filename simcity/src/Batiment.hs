{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Batiment where
import Forme
import Types
import Citoyen
import Prefecture
import Occupation -- can be deleted
import Produit
import Vehicule
import Entreprise
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
                    clients :: [CitId],  -- Liste des citoyens clients de cette épicerie
                    stock :: StockProduit

                }
              | Commissariat {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    heliport :: Bool
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
                    patients :: [CitId],  -- Liste des citoyens patients de cet hôpital,
                    heliport :: Bool
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
                    clients :: [CitId]  -- Liste des citoyens clients de ce restaurant de la journée
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
              | Maison {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    capacite :: Int,  -- Capacité maximale de citoyens que cette maison peut contenir
                    habitants :: [CitId]  -- Liste des citoyens habitant cette maison
                }
              | CasernePompier {
                    forme :: Forme,
                    zoneId :: ZonId,
                    entree :: Coord,
                    pompiers :: Int, -- nombre de pompiers
                    camions :: Int, -- nombre de camions
                    heliport :: Bool

                }
              | Entrep {
                   entrep :: Entreprise,
                   forme :: Forme,
                   zoneId :: ZonId,
                   entree :: Coord
              }

                deriving(Eq)


-- instances

instance Show Batiment where
    show (Cabane forme zoneId _ capacite habitants) = "Cabane " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length habitants) ++ " habitants"
    show (Atelier forme zoneId _ capacite employes) = "Atelier " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length employes) ++ " employés"
    show (Epicerie forme zoneId _ capacite clients  s) = "Epicerie " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length clients) ++ " clients" ++  " et " ++ show s ++ " stock"
    show (Commissariat forme zoneId _ h) = "Commissariat " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec un heliport " ++ show h
    show (Ecole forme zoneId _ capacite eleves) = "Ecole " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length eleves) ++ " élèves"
    show (Hopital forme zoneId _ capacite patients h) = "Hopital " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length patients) ++ " patients" ++ " et un heliport " ++ show h
    show (Cinema forme zoneId _ capacite spectateurs) = "Cinema " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length spectateurs) ++ " spectateurs"
    show (Restaurant forme zoneId _ capacite clients) = "Restaurant " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length clients) ++ " clients"
    show (Banque idBanque forme zoneId _ argent clients) = "Banque " ++ show idBanque ++ " " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec un argent de " ++ show argent ++ " et " ++ show (length clients) ++ " clients"
    show (Pref pref forme zoneId _) = "Prefecture " ++ show pref ++ " " ++ show forme ++ " dans la zone " ++ show zoneId
    show (Maison forme zoneId _ capacite habitants) = "Maison " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length habitants) ++ " habitants"
    show (CasernePompier forme zoneId _ pompiers camions heliport) = "Caserne de Pompier " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec " ++ show pompiers ++ " pompiers et " ++ show camions ++ " camions et un heliport " ++ show heliport
    show (Entrep entrep forme zoneId _) = "Entreprise " ++ show entrep ++ " " ++ show forme ++ " dans la zone " ++ show zoneId
-- fonctions pour gérer notre batiment
-- on a supposé que les seul modifications possibles sont les cas dessus car c'est plutot pas trop réel de changer le type d'un batiment ou son entrée
-- voir qu'il y a des cas dans la vraie vie où on peut changer le type d'un batiment mais ce n'est pas un généralité
ajouterCapacite :: Batiment -> Int -> Batiment
ajouterCapacite (Cabane forme zoneId entree capacite habitants) n = Cabane forme zoneId entree (capacite + n) habitants
ajouterCapacite (Atelier forme zoneId entree capacite employes) n = Atelier forme zoneId entree (capacite + n) employes
ajouterCapacite (Epicerie forme zoneId entree capacite clients  s) n = Epicerie forme zoneId entree (capacite + n) clients  s
ajouterCapacite (Commissariat forme zoneId entree h) _ = Commissariat forme zoneId entree h
ajouterCapacite (Ecole forme zoneId entree capacite eleves) n = Ecole forme zoneId entree (capacite + n) eleves
ajouterCapacite (Hopital forme zoneId entree capacite patients h) n = Hopital forme zoneId entree (capacite + n) patients h
ajouterCapacite (Cinema forme zoneId entree capacite spectateurs) n = Cinema forme zoneId entree (capacite + n) spectateurs
ajouterCapacite (Restaurant forme zoneId entree capacite clients) n = Restaurant forme zoneId entree (capacite + n) clients



diminuerCapacite :: Batiment -> Int -> Batiment
diminuerCapacite (Cabane forme zoneId entree capacite habitants) n = Cabane forme zoneId entree (max (capacite - n) 0) habitants
  --                                                                                        le max c'est juste pour ne pas avoir des valeurs négatives
diminuerCapacite (Atelier forme zoneId entree capacite employes) n = Atelier forme zoneId entree (max (capacite - n) 0) employes
diminuerCapacite (Epicerie forme zoneId entree capacite clients  s) n = Epicerie forme zoneId entree (max (capacite - n) 0) clients  s
diminuerCapacite (Commissariat forme zoneId entree h) _ = Commissariat forme zoneId entree h
diminuerCapacite (Ecole forme zoneId entree capacite eleves) n = Ecole forme zoneId entree (max (capacite - n) 0) eleves
diminuerCapacite (Hopital forme zoneId entree capacite patients h) n = Hopital forme zoneId entree (max (capacite - n) 0) patients h
diminuerCapacite (Cinema forme zoneId entree capacite spectateurs) n = Cinema forme zoneId entree (max (capacite - n) 0) spectateurs
diminuerCapacite (Restaurant forme zoneId entree capacite clients) n = Restaurant forme zoneId entree (max (capacite - n) 0) clients

ajouterCitoyen :: Batiment -> CitId -> Batiment
ajouterCitoyen (Cabane forme zoneId entree capacite habitants) citId = Cabane forme zoneId entree capacite (citId:habitants)
ajouterCitoyen (Atelier forme zoneId entree capacite employes) citId = Atelier forme zoneId entree capacite (citId:employes)
ajouterCitoyen (Epicerie forme zoneId entree capacite clients  s) citId = Epicerie forme zoneId entree capacite (citId:clients)  s
ajouterCitoyen (Commissariat forme zoneId entree h) _ = Commissariat forme zoneId entree h
ajouterCitoyen (Ecole forme zoneId entree capacite eleves) citId = Ecole forme zoneId entree capacite (citId:eleves)
ajouterCitoyen (Maison forme zoneId entree capacite habitants) citId = Maison forme zoneId entree capacite (citId:habitants)
-- NB : pour l'hopital, on a une fonction spécifique pour ajouter un patient (voir cf . hospitaliser)

retirerCitoyen :: Batiment -> CitId -> Batiment
retirerCitoyen (Cabane forme zoneId entree capacite habitants) citId = Cabane forme zoneId entree capacite (filter (/= citId) habitants)
retirerCitoyen (Atelier forme zoneId entree capacite employes) citId = Atelier forme zoneId entree capacite (filter (/= citId) employes)
retirerCitoyen (Epicerie forme zoneId entree capacite clients  s) citId = Epicerie forme zoneId entree capacite (filter (/= citId) clients)  s
retirerCitoyen (Commissariat forme zoneId entree h) _ = Commissariat forme zoneId entree h
retirerCitoyen (Ecole forme zoneId entree capacite eleves) citId = Ecole forme zoneId entree capacite (filter (/= citId) eleves)
retirerCitoyen (Maison forme zoneId entree capacite habitants) citId = Maison forme zoneId entree capacite (filter (/= citId) habitants)

-- creer des batiments
creerMaison :: Forme -> ZonId -> Coord -> Int -> Batiment
creerMaison forme zoneId entree capacite = Maison forme zoneId entree capacite []

creerCabane :: Forme -> ZonId -> Coord -> Int -> Batiment
creerCabane forme zoneId entree capacite = Cabane forme zoneId entree capacite []

creerAtelier :: Forme -> ZonId -> Coord -> Int -> Batiment
creerAtelier forme zoneId entree capacite = Atelier forme zoneId entree capacite []

creerEpicerie :: Forme -> ZonId -> Coord -> Int -> Batiment
creerEpicerie forme zoneId entree capacite = Epicerie forme zoneId entree capacite [] (StockProduit [])

creerCommissariat :: Forme -> ZonId -> Coord -> Batiment
creerCommissariat forme zoneId entree = Commissariat forme zoneId entree True

creerEcole :: Forme -> ZonId -> Coord -> Int -> Batiment
creerEcole forme zoneId entree capacite = Ecole forme zoneId entree capacite []

creerHopital :: Forme -> ZonId -> Coord -> Int -> Batiment
creerHopital forme zoneId entree capacite = Hopital forme zoneId entree capacite [] True

creerCinema :: Forme -> ZonId -> Coord -> Int -> Batiment
creerCinema forme zoneId entree capacite = Cinema forme zoneId entree capacite []

creerRestaurant :: Forme -> ZonId -> Coord -> Int -> Batiment
creerRestaurant forme zoneId entree capacite = Restaurant forme zoneId entree capacite []

creerBanque :: BankId -> Forme -> ZonId -> Coord -> Float -> Batiment
creerBanque idBanque forme zoneId entree argent = Banque idBanque forme zoneId entree argent []

creerPrefecture :: Prefecture -> Forme -> ZonId -> Coord -> Batiment
creerPrefecture = Pref

-- gérer les pompiers
ajouterNombrePompier :: Batiment -> Int -> Batiment
ajouterNombrePompier (CasernePompier forme zoneId entree pompiers camions heliport) n = CasernePompier forme zoneId entree (pompiers + n) camions heliport

ajouterNombreCamion :: Batiment -> Int -> Batiment
ajouterNombreCamion (CasernePompier forme zoneId entree pompiers camions heliport) n = CasernePompier forme zoneId entree pompiers (camions + n) heliport

diminuerNombrePompier :: Batiment -> Int -> Batiment
diminuerNombrePompier (CasernePompier forme zoneId entree pompiers camions heliport) n = CasernePompier forme zoneId entree (max (pompiers - n) 0) camions heliport

diminuerNombreCamion :: Batiment -> Int -> Batiment
diminuerNombreCamion (CasernePompier forme zoneId entree pompiers camions heliport) n = CasernePompier forme zoneId entree pompiers (max (camions - n) 0) heliport




-- Banque


virementBancaire :: Batiment -> Float -> Batiment->Citoyen-> Citoyen-> (Batiment,Batiment, Citoyen, Citoyen)
virementBancaire b1 montant b2 emetteur recepteur
  | Citoyen.getCitoyenId emetteur `notElem` getClientsBanque b1 = error "l'emetteur n'est pas un client de la banque"
  | Citoyen.getCitoyenId recepteur `notElem` getClientsBanque b2 = error "le recepteur n'est pas un client de la banque"
  | otherwise = let b1'= getFstMaybe (virementBanqueVersBanque b1 montant b2) in
                let b2' = getSndMaybe (virementBanqueVersBanque b1 montant b2) in
                let emetteur' = getFstMaybe (virementCitoyenVersCitoyen emetteur montant recepteur) in
                let recepteur' = getSndMaybe (virementCitoyenVersCitoyen emetteur montant recepteur) in
                 if b1 == b2 then (b1,b1,emetteur',recepteur') -- si je suis dans la même banque je ne change rien dans le montant de la banque, je change juste le montant des des deux comptes
                 else  (b1', b2', emetteur', recepteur')



virementBanqueVersBanque :: Batiment -> Float -> Batiment -> Maybe (Batiment, Batiment)
virementBanqueVersBanque (Banque id forme zoneId entree argent clients) montant (Banque id' forme' zoneId' entree' argent' clients') =
        if argent - montant < 0 then Nothing
        else Just (Banque id forme zoneId entree (argent - montant) clients, Banque id' forme' zoneId' entree' (argent' + montant) clients')

virementCitoyenVersCitoyen :: Citoyen -> Float -> Citoyen-> Maybe (Citoyen,Citoyen)
virementCitoyenVersCitoyen (Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp)
                    montant
                  (Habitant (Personne id' c' o' cri' nat' m') (Vie argent' sante' nivFaim' nivFatigue') vp') =
                    if argent < montant then Nothing
                     else Just (Habitant (Personne id c o cri nat m) (Vie (argent - montant) sante nivFaim nivFatigue) vp, Habitant (Personne id' c' o' cri' nat' m') (Vie (argent' + montant) sante' nivFaim' nivFatigue') vp')

-- recuperer les valeurs du maybe
getFstMaybe :: Maybe (a,a) -> a
getFstMaybe (Just (first,_)) = first
getFstMaybe Nothing = error "Nothing to get"

getSndMaybe :: Maybe (a,b) -> b
getSndMaybe (Just (_,b)) = b
getSndMaybe Nothing = error "Nothing to get"

-- getCitoyenId :: Citoyen -> CitId
-- getCitoyenId (Habitant (Personne id _ _ _ _ _) _ _) = id

getClientsBanque :: Batiment -> [CitId]
getClientsBanque (Banque _ _ _ _ _ clients) = clients


-- ! invariants 


invariantBatiment :: Batiment -> Bool
invariantBatiment b = condBatiment b && capacitePositive b


condBatiment :: Batiment -> Bool
condBatiment (Cabane _ _ _ capacite habitants) = length habitants <= capacite
condBatiment (Atelier _ _ _ capacite employes) = length employes <= capacite
condBatiment (Epicerie _ _ _ capacite clients  _) = length clients <= capacite
condBatiment (Ecole _ _ _ capacite eleves) = length eleves <= capacite
condBatiment (Hopital _ _ _ capacite patients _) = length patients <= capacite
condBatiment (Cinema _ _ _ capacite spectateurs) = length spectateurs <= capacite
condBatiment (Restaurant _ _ _ capacite clients) = length clients <= capacite
condBatiment _ = error "ce batiment n'a pas de capacité"

-- * la capacité d'un batiment ne peut pas être négative ou  infinie
capacitePositive :: Batiment -> Bool
capacitePositive (Cabane _ _ _ capacite _) = capacite > 0 ||  capacite /= maxBound
capacitePositive (Atelier _ _ _ capacite _) = capacite > 0   ||  capacite /= maxBound
capacitePositive (Epicerie _ _ _ capacite _  _) = capacite > 0  ||  capacite /= maxBound
capacitePositive (Ecole _ _ _ capacite _) = capacite > 0  ||  capacite /= maxBound
capacitePositive (Hopital _ _ _ capacite _ _) = capacite > 0  ||  capacite /= maxBound
capacitePositive (Cinema _ _ _ capacite _) = capacite > 0  ||  capacite /= maxBound
capacitePositive (Restaurant _ _ _ capacite _) = capacite > 0  ||  capacite /= maxBound
capacitePositive _ = error "ce batiment n'a pas de capacité"

getCapacite :: Batiment -> Int
getCapacite (Cabane _ _ _ capacite _) = capacite
getCapacite (Atelier _ _ _ capacite _) = capacite
getCapacite (Epicerie _ _ _ capacite _ _) = capacite
getCapacite (Ecole _ _ _ capacite _) = capacite
getCapacite (Hopital _ _ _ capacite _ _) = capacite

ajouterPatient :: Batiment -> CitId -> Batiment
ajouterPatient (Hopital f zid e capacite patients h) citId =
                    let size = length patients in
                    if size < capacite then
                      if citId `elem` patients then
                            error "le citoyen est déjà hospitalisé"
                      else
                            Hopital f zid e capacite (citId:patients) h
                    else error "le batiment est plein"

hospitaliser :: Citoyen -> Batiment -> Batiment
hospitaliser (Habitant (Personne id c o cri nat m) (Vie argent sante nivFaim nivFatigue) vp) bat =
              case bat of
                Hopital f zid e capacite patients h ->
                            ajouterPatient (Hopital f zid e capacite patients h) id
                _ -> error "ce n'est pas un hopital"

sortirPatientDeL'Hopital :: CitId -> Batiment -> Batiment
sortirPatientDeL'Hopital citId (Hopital f zid e capacite patients h) =
          if citId `elem` patients then
           Hopital f zid e capacite (filter (/= citId) patients) h
           else error "le citoyen n'est pas hospitalisé dans cette hopital"


-- vente de produit
vendreProduit :: Batiment -> Produit -> (Batiment,Produit)
vendreProduit (Epicerie f zid e capacite clients stock) produit =
              let (foundProduct,foundQuantity) = getProduitQuantite stock produit in
             if foundQuantity == Quantite 0 then error "This product is out of stock"
             else
                if foundQuantity >= Quantite 1 then
                    (Epicerie f zid e capacite clients (removeProduct stock produit), foundProduct)
                else
                    error "Valeur Negative"




