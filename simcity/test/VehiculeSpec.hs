{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module VehiculeSpec where 

import Test.Hspec
import Vehicule
import Types
import Control.Exception (evaluate)

import qualified Distribution.Compat.Prelude as Prelude
vehic1 = Vehicule (VehicId 1) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1))) [] 1000 5

vehic2 = Vehicule (VehicId 2) Voiture "12534" (Just (VehiculeCitoyen (CitId 1))) [] 1000 5
vehic3 = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 2))) [] 1000 5
vehic4 = Vehicule (VehicId 4) Voiture "12534" Nothing [CitId 1] 1000 5

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



vehiculeSpec :: Spec
vehiculeSpec = do
        describe "vehiculeParCitoyen" $ do
          it "vérifie si un citoyen est propriétaire d'un véhicule" $ do
            evaluate (vehiculeParCitoyen vehic2 (CitId 1)) `shouldThrow` errorCall "Ce citoyen est deja proprietaire de ce vehicule"
            evaluate (vehiculeParCitoyen vehic1 (CitId 1)) `shouldThrow` errorCall "Ce vehicule appartient a une entreprise et ne peut pas etre vendu a un citoyen"

        describe "vehiculeParEntreprise" $ do
             it "vérifie si une entreprise est propriétaire d'un véhicule" $ do
              evaluate nvehiEntre `shouldThrow`errorCall "Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même"
        -- all good
        describe "achatVehicule" $ do
          it "vérifie l'achat de véhicule par un citoyen" $ do
                 evaluate nvehiCit `shouldThrow` errorCall "Ce citoyen est deja proprietaire de ce vehicule"
                 show nvehiCit' `shouldBe`  "Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}"
          it "vérifie l'achat de véhicule sans propriétaire" $ do
            show nvehicNothing `shouldBe` "Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}"
            show nvehicNothing' `shouldBe` "Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}"
          it "vérifie l'achat de véhicule par une entreprise" $ do
            evaluate nvehiEntre `shouldThrow` errorCall "Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même"
            show nvehiEntre' `shouldBe` "Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0, capacitePassagers = 5}"

        describe "ajouterPassagers" $ do
          it "ajout des passagers à un véhicule" $ do
              show (ajouterPassagers vehic3 [CitId 2, CitId 3]) `shouldBe` "Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [CitId 2,CitId 3], prixVehic = 1000.0, capacitePassagers = 5}"
              show (ajouterPassagers vehic3 [CitId 2, CitId 3]) `shouldBe` "Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = \"12534\", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [CitId 2,CitId 3], prixVehic = 1000.0, capacitePassagers = 5}"
          it "vérifie les erreurs lors de l'ajout des passagers" $ do
              evaluate (ajouterPassagers vehic3 [CitId 2, CitId 3, CitId 4, CitId 5, CitId 6, CitId 8, CitId 7]) `shouldThrow` errorCall "Le nombre de passagers depasse la capacite du vehicule"
              evaluate (ajouterPassagers vehic3 [CitId 3, CitId 3]) `shouldThrow` errorCall "Un passager ne peut pas etre ajouté deux fois"
          it "ajout des passagers à un véhicule sans propriétaire" $ do
          --     -- evaluate (ajouterPassagers vehic4 [CitId 2]) `shouldThrow` errorCall "Ce vehicule n'a toujours pas de propritaire, seul le propriataire peut ajouter des passagers"
               evaluate (ajouterPassagers vehic4 [CitId 2]) `shouldThrow` errorCall  "Ce vehicule n'a toujours pas de propritaire, seul le proprietaire peut ajouter des passagers"