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
