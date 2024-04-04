module Batiment where
import Forme
import Types

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
                deriving(Eq)


-- instances

instance Show Batiment where
    show (Cabane forme zoneId _ capacite habitants) = "Cabane " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length habitants) ++ " habitants"
    show (Atelier forme zoneId _ capacite employes) = "Atelier " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length employes) ++ " employés"
    show (Epicerie forme zoneId _ capacite clients) = "Epicerie " ++ show forme ++ " dans la zone " ++ show zoneId ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length clients) ++ " clients"
    show (Commissariat forme zoneId _) = "Commissariat " ++ show forme ++ " dans la zone " ++ show zoneId

-- fonctions pour gérer notre batiment
-- on a supposé que les seul modifications possibles sont les cas dessus car c'est plutot pas trop réel de changer le type d'un batiment ou son entrée
-- voir qu'il y a des cas dans la vraie vie où on peut changer le type d'un batiment mais ce n'est pas un généralité
ajouterCapacite :: Batiment -> Int -> Batiment
ajouterCapacite (Cabane forme zoneId entree capacite habitants) n = Cabane forme zoneId entree (capacite + n) habitants
ajouterCapacite (Atelier forme zoneId entree capacite employes) n = Atelier forme zoneId entree (capacite + n) employes
ajouterCapacite (Epicerie forme zoneId entree capacite clients) n = Epicerie forme zoneId entree (capacite + n) clients
ajouterCapacite (Commissariat forme zoneId entree) _ = Commissariat forme zoneId entree

diminuerCapacite :: Batiment -> Int -> Batiment
diminuerCapacite (Cabane forme zoneId entree capacite habitants) n = Cabane forme zoneId entree (max (capacite - n) 0) habitants
  --                                                                                        le max c'est juste pour ne pas avoir des valeurs négatives
diminuerCapacite (Atelier forme zoneId entree capacite employes) n = Atelier forme zoneId entree (max (capacite - n) 0) employes
diminuerCapacite (Epicerie forme zoneId entree capacite clients) n = Epicerie forme zoneId entree (max (capacite - n) 0) clients
diminuerCapacite (Commissariat forme zoneId entree) _ = Commissariat forme zoneId entree

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

