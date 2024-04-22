module VehiculeTest where

import Vehicule
import Types
import qualified Distribution.Compat.Prelude as Prelude

vehic1 = Vehicule (VehicId 1) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1))) [] 1000 5

vehic2 = Vehicule (VehicId 2) Voiture "12534" (Just (VehiculeCitoyen (CitId 1))) [] 1000 5
vehic3 = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 2))) [] 1000 5
vehic4 = Vehicule (VehicId 4) Voiture "12534" Nothing [CitId 1] 1000 5



-- >>> vehiculeParCitoyen vehic2 (CitId 1)
-- Ce citoyen est deja proprietaire de ce vehicule

-- >>> vehiculeParCitoyen vehic1 (CitId 1)
-- Ce vehicule appartient a une entreprise et ne peut pas etre vendu a un citoyen

-- >>> vehiculeParEntreprise vehic1 (EntrepriseId 1)
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même

-- vehic3 = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1))) [] 1000
vehiCit = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeCitoyen (CitId 1) )) [] 1000 5
vehicNothing = Vehicule (VehicId 3) Voiture "12534" Nothing [] 1000 5
vehiEntre = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1) )) [] 1000 5

-- modifications
-- citoyens
nvehiCit = achatVehicule vehiCit (VehiculeCitoyen (CitId 1))
nvehiCit' = achatVehicule vehiCit (VehiculeCitoyen (CitId 2))
-- nothing
nvehicNothing = achatVehicule vehicNothing (VehiculeCitoyen (CitId 2))
nvehicNothing' = achatVehicule vehicNothing (VehiculeEntreprise (EntrepriseId 2))

-- entreprises 
nvehiEntre = achatVehicule vehiEntre (VehiculeEntreprise (EntrepriseId 1))
nvehiEntre' = achatVehicule vehiEntre (VehiculeEntreprise (EntrepriseId 2))

-- >>> nvehiCit
-- Ce citoyen est deja proprietaire de ce vehicule

-- >>> nvehiCit'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}

-- >>> nvehicNothing
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}

-- >>> nvehicNothing'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}

-- >>> nvehiEntre
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même

-- >>> nvehiEntre'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}


-- >>> ajouterPassagers vehic4 [CitId 2]
-- Ce vehicule n'a toujours pas de propritaire, seul le proprietaire peut ajouter des passagers

-- >>> ajouterPassagers vehic3 [CitId 2, CitId 3, CitId 4, CitId 5, CitId 6,CitId 8,CitId 7]
-- Le nombre de passagers depasse la capacite du vehicule

-- >>> ajouterPassagers vehic3 [CitId 2, CitId 3]
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [CitId 2,CitId 3], prixVehic = 1000.0, capacitePassagers = 5}

-- >>> ajouterPassagers vehic3 [CitId 3, CitId 3] 
-- Un passager ne peut pas etre ajouté deux fois
