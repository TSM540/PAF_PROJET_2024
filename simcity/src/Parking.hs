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
enleverVoitureDuParking v (ParkingMaison c vs pid) = 
                if v `elem` vs then 
                    ParkingMaison c (filter (/=v) vs) pid
                else error "La voiture n'est pas dans le parking maison"
enleverVoitureDuParking v (ParkingImmeuble c vs id) = 
                if v `elem` vs then    
                        ParkingImmeuble c (filter (/=v) vs) id
                else error "La voiture n'est pas dans le parking immeuble"


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
parkingMaisonA6PlacesAuPlus _ = True


invariantParking :: Parking -> Bool
invariantParking p@(ParkingMaison (Capacite _) _ _) = parkingDisponible p
                                            && parkingMaisonA6PlacesAuPlus p

invariantParking p@(ParkingImmeuble (Capacite _) _ _) = parkingDisponible p


