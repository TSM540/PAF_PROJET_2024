module ParkingSpec where 

import Test.Hspec
import Parking
import Control.Exception (evaluate)
import Types 


parking :: Parking
parking = ParkingMaison (Capacite 6) [VehicId 1] (ParkingId 1)
parking2 :: Parking
parking2 = ParkingImmeuble (Capacite 6) [VehicId 1] (ParkingId 1)

parkingSpec :: Spec
parkingSpec = do 
    describe "affichage d'un parking " $ do 
        it "affiche un parking maison" $ do
            show parking `shouldBe` "Parking maison de capacit\233 Capacite 6 avec 1 v\233hicules et id ParkingId 1"
        it "affiche un parking immeuble" $ do
            show parking2 `shouldBe` "Parking immeuble de capacit\233 Capacite 6 avec 1 v\233hicules et id ParkingId 1"
    describe "ajouterVoitureAuParking" $ do
        it "ajoute une voiture au parking maison valide" $ do
            show (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 6) [] (ParkingId 1))) `shouldBe` "Just Parking maison de capacit\233 Capacite 6 avec 1 v\233hicules et id ParkingId 1"
        it "ajoute une voiture au parking maison valide " $ do
            show (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 6) [VehicId 1] (ParkingId 1))) `shouldBe` "Just Parking maison de capacit\233 Capacite 6 avec 2 v\233hicules et id ParkingId 1"
        it "capacité atteinte" $ do
            evaluate (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldThrow` errorCall "Le parking plein"
        it "capacité négative" $ do
            evaluate (ajouterVoitureAuParking (VehicId 1) (ParkingMaison (Capacite (-1)) [] (ParkingId 1))) `shouldThrow` errorCall "la capacit\233 est nulle ou n\233gative"
    describe "parkingVide" $ do
        it "parking vide" $ do
            parkingVide (ParkingMaison (Capacite 6) [] (ParkingId 1)) `shouldBe` True
        it "parking non vide" $ do
            parkingVide (ParkingMaison (Capacite 6) [VehicId 1] (ParkingId 1)) `shouldBe` False
    describe "parking A plus de 6 places" $ do
        it "parking maison a plus de 6 places" $ do
            evaluate (parkingMaisonA6PlacesAuPlus (ParkingMaison (Capacite 7) [] (ParkingId 1))) `shouldThrow` errorCall "Le parking maison ne peut pas avoir plus de 6 places"
        it "parking maison a 6 places" $ do
            parkingMaisonA6PlacesAuPlus (ParkingMaison (Capacite 6) [] (ParkingId 1)) `shouldBe` True
    describe "enleverVoitureDuParking" $ do
        it "enleve une voiture du parking maison" $ do
            show(enleverVoitureDuParking (VehicId 1) (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldBe` "Parking maison de capacit\233 Capacite 6 avec 5 v\233hicules et id ParkingId 1"
        it "enleve une voiture du parking immeuble" $ do
            show(enleverVoitureDuParking (VehicId 1) (ParkingImmeuble (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldBe` "Parking immeuble de capacit\233 Capacite 6 avec 5 v\233hicules et id ParkingId 1"
        it "la voiture n'est pas dans le parking maison" $ do
            evaluate (enleverVoitureDuParking (VehicId 7) (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1)))
                    `shouldThrow`  errorCall "La voiture n'est pas dans le parking maison"
        it "la voiture n'est pas dans le parking immeuble" $ do
            evaluate (enleverVoitureDuParking (VehicId 7) (ParkingImmeuble (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldThrow` errorCall "La voiture n'est pas dans le parking immeuble"
    describe "parkingDisponible" $ do
        it "parking maison disponible" $ do
            parkingDisponible (ParkingMaison (Capacite 6) [] (ParkingId 1)) `shouldBe` True
        it "parking maison plein" $ do
            evaluate (parkingDisponible (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldThrow` errorCall "Le parking plein"
        it "parking maison capacité négative" $ do
            evaluate (parkingDisponible (ParkingMaison (Capacite (-1)) [] (ParkingId 1))) `shouldThrow` errorCall "la capacit\233 est nulle ou n\233gative"
        it "parking immeuble disponible" $ do
            parkingDisponible (ParkingImmeuble (Capacite 6) [] (ParkingId 1)) `shouldBe` True
        it "parking immeuble plein" $ do
            evaluate (parkingDisponible (ParkingImmeuble (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldThrow` errorCall "Le parking plein"
        it "parking immeuble capacité négative" $ do
            evaluate (parkingDisponible (ParkingImmeuble (Capacite (-1)) [] (ParkingId 1))) `shouldThrow` errorCall "la capacit\233 est nulle ou n\233gative"
    describe "invariantParking" $ do
        it "invariant parking maison" $ do
            evaluate (invariantParking (ParkingMaison (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldThrow` errorCall "Le parking plein"
        it "invariant parking immeuble" $ do
            evaluate (invariantParking (ParkingImmeuble (Capacite 6) [VehicId 1, VehicId 2, VehicId 3, VehicId 4, VehicId 5, VehicId 6] (ParkingId 1))) `shouldThrow` errorCall "Le parking plein"
        it "invariant parking maison capacité négative" $ do
            evaluate (invariantParking (ParkingMaison (Capacite (-1)) [] (ParkingId 1))) `shouldThrow` errorCall "la capacit\233 est nulle ou n\233gative"
        it "invariant parking immeuble capacité négative" $ do
            evaluate (invariantParking (ParkingImmeuble (Capacite (-1)) [] (ParkingId 1))) `shouldThrow` errorCall "la capacit\233 est nulle ou n\233gative"
        it "invariant parking maison a plus de 6 places" $ do
            evaluate (invariantParking (ParkingMaison (Capacite 7) [] (ParkingId 1))) `shouldThrow` errorCall "Le parking maison ne peut pas avoir plus de 6 places"
        it "invariant parking maison disponible" $ do
            invariantParking (ParkingMaison (Capacite 6) [] (ParkingId 1)) `shouldBe` True
        it "invariant parking immeuble disponible" $ do
            invariantParking (ParkingImmeuble (Capacite 6) [] (ParkingId 1)) `shouldBe` True
        it "invariant parking maison a 6 places" $ do
            invariantParking (ParkingMaison (Capacite 6) [] (ParkingId 1)) `shouldBe` True
       