module Vehicule where 
import Types

data Vehicule = Vehicule {
    idVehic :: VehicId,
    typeVehic :: TypeVehicule,
    immatriculation :: String,
    propriataire :: Maybe CitId,
    passagers :: [CitId],
    prixVehic :: Float
} deriving (Show, Eq)

data TypeVehicule = Voiture
                    | Moto
                    | Camion
                    | Bus
                    | Helicoptere
                     deriving (Show, Eq)

    
-- acheterVehicule :: Vehicule ->
