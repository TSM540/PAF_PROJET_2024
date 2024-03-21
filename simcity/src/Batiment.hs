module Batiment where
import Forme 
import Types

data Batiment =   Cabane Forme Coord Int [CitId]
                | Atelier Forme Coord Int [CitId]
                | Epicerie Forme Coord Int [CitId]
                | Commissariat Forme Coord

-- instances

instance Show Batiment where
    show (Cabane forme coord capacite citoyens) = "Cabane " ++ show forme ++ " à " ++ show coord ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length citoyens) ++ " citoyens"
    show (Atelier forme coord capacite citoyens) = "Atelier " ++ show forme ++ " à " ++ show coord ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length citoyens) ++ " citoyens"
    show (Epicerie forme coord capacite citoyens) = "Epicerie " ++ show forme ++ " à " ++ show coord ++ " avec une capacité de " ++ show capacite ++ " et " ++ show (length citoyens) ++ " citoyens"
    show (Commissariat forme coord) = "Commissariat " ++ show forme ++ " à " ++ show coord
