module Parking where 

import Types 


instance Show Parking where
    show (ParkingMaison  c v id) = "Parking maison de capacité " ++ show c ++ " avec " ++ show (length v) ++ " véhicules" ++ " et id " ++ show id
    show (ParkingImmeuble  c v id) = "Parking immeuble de capacité " ++ show c ++ " avec " ++ show (length v) ++ " véhicules" ++ " et id " ++ show id


ajouterVoitureAuParking :: VehicId -> Parking ->  Maybe Parking
ajouterVoitureAuParking v oldparking@(ParkingMaison c vs id) = 
                                                   if parkingDisponible oldparking then 
                                                         let newparking = ParkingMaison c (v:vs) id in
                                                        let boolInv = invariantParking newparking in
                                                        if boolInv then 
                                                             Just (ParkingMaison c (v:vs) id) 
                                                        else Nothing
                                                    else Nothing
ajouterVoitureAuParking v (ParkingImmeuble c vs id) = if invariantParking (ParkingImmeuble c (v:vs) id) then Just (ParkingImmeuble c (v:vs) id ) else Nothing

enleverVoitureDuParking :: VehicId -> Parking -> Parking
enleverVoitureDuParking v (ParkingMaison c vs id) = ParkingMaison c (filter (/=v) vs) id
enleverVoitureDuParking v (ParkingImmeuble c vs id) = ParkingImmeuble c (filter (/=v) vs) id


parkingVide :: Parking -> Bool
parkingVide (ParkingMaison _ [] _) = True
parkingVide (ParkingImmeuble _ [] _) = True
parkingVide _ = False

parkingPlein :: Parking -> Bool
parkingPlein (ParkingMaison (Capacite c) vs _) = length vs == c 
parkingPlein (ParkingImmeuble (Capacite c) vs _) = length vs == c 

parkingDisponible :: Parking -> Bool
parkingDisponible p@(ParkingMaison (Capacite c) vs _) = (length vs < c && c > 0) || if parkingPlein p then error "Le parking plein" else error "la capacité est nulle ou négative"
parkingDisponible p@(ParkingImmeuble (Capacite c) vs _) = length vs < c && c > 0 || if parkingPlein p then error "Le parking plein" else error "la capacité est nulle ou négative"

parkingMaisonA6PlacesAuPlus :: Parking -> Bool
parkingMaisonA6PlacesAuPlus (ParkingMaison (Capacite c) _ _) = c <= 6 || if c > 6 then error "Le parking maison ne peut pas avoir plus de 6 places" else error "la capacité est nulle ou négative"



invariantParking :: Parking -> Bool
invariantParking p@(ParkingMaison (Capacite c) vs _) = parkingDisponible p
                                            && parkingMaisonA6PlacesAuPlus p

invariantParking p@(ParkingImmeuble (Capacite c) vs _) = parkingDisponible p


parking = ParkingMaison (Capacite 6) [VehicId 1] (ParkingId 1)
parking2 = ParkingImmeuble (Capacite 6) [VehicId 1] (ParkingId 1)

-- >>> show (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 6) [] (ParkingId 1)))
-- "Just Parking maison de capacit\233 Capacite 6 avec 1 v\233hicules et id ParkingId 1"

-- >>> show (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 6) [VehicId 1] (ParkingId 1)))
-- "Just Parking maison de capacit\233 Capacite 6 avec 2 v\233hicules et id ParkingId 1"

-- >>> show (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1)))
-- Le parking plein

-- >>> show (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 0) [] (ParkingId 1)))
-- Le parking plein

-- >>> show(enleverVoitureDuParking (VehicId 1) (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1)))
-- "Parking maison de capacit\233 Capacite 6 avec 5 v\233hicules et id ParkingId 1"
