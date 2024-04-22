module ParkingTest where

import Parking

import Types 


parking = ParkingMaison (Capacite 6) [VehicId 1] (ParkingId 1)
parking2 = ParkingImmeuble (Capacite 6) [VehicId 1] (ParkingId 1)

-- >>> show parking
-- "Parking maison de capacit\233 Capacite 6 avec 1 v\233hicules et id ParkingId 1"

-- >>> show parking2
-- "Parking immeuble de capacit\233 Capacite 6 avec 1 v\233hicules et id ParkingId 1"

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
