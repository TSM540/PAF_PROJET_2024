module EntrepriseSpec where
import Test.Hspec
import Entreprise
import Types
import FormeSpec
import VehiculeSpec
import Control.Exception (evaluate, ErrorCall (ErrorCall), SomeException (SomeException))
import Vehicule


entreprise1 :: Entreprise
entreprise1 = Entreprise (EntrepriseId 1)  [BatId 1] [employe1] 1000 []

entreprise2 :: Entreprise
entreprise2 = Entreprise (EntrepriseId 2)  [BatId 2,BatId 1] [employe2,employe3] 1000 []

entreprise3 :: Entreprise
entreprise3 = Entreprise (EntrepriseId 3)  [BatId 3] [employe1] 1000 []

entreprise4 :: Entreprise
entreprise4 = Entreprise (EntrepriseId 4)  [BatId 4] [employe1] 1000 []
entreprise5 :: Entreprise
entreprise5 = Entreprise (EntrepriseId 4)  [BatId 4] [employe1] 0 []
entreprise6 :: Entreprise
entreprise6 = Entreprise (EntrepriseId 4)  [BatId 4] [] 1000 []
entreprise7 :: Entreprise
entreprise7 = Entreprise (EntrepriseId 4)  [] [employe1] 1000 []
entreprise8 :: Entreprise
entreprise8 = Entreprise (EntrepriseId 4)  [BatId 4] [employe1,employe2] 1000 []

entreprise9 :: Entreprise
entreprise9 = Entreprise (EntrepriseId 4)  [BatId 4] [employe3] 1000 []
-- les employés
employe1 :: (CitId, Poste)
employe1 = (CitId 1,CEO)
employe2 :: (CitId, Poste)
employe2 = (CitId 2,CEO)
employe3 :: (CitId, Poste)
employe3 = (CitId 3,ProductManager)
employe4 :: (CitId, Poste)
employe4 = (CitId 4,CTO)


nefalse :: Maybe Entreprise
nefalse = uncurry (recruterEmploye entreprise1) employe2
netrue :: Maybe Entreprise
netrue = uncurry (recruterEmploye entreprise1) employe3

entrepriseSpec :: Spec
entrepriseSpec = do
    describe "recruterEmploye" $ do
        it "Un seul CEO" $ do
            evaluate nefalse `shouldThrow` errorCall "Votre entreprise doit avoir un seul CEO"
        it "recruter un employé" $ do
            netrue `shouldBe` Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 3,ProductManager),(CitId 1,CEO)], capital = 1000.0, vehiculesService = []})

    describe "virerEmploye" $ do
        it "l'entreprise n'aura plus d'employé " $ do
            evaluate (virerEmploye entreprise2 (fst employe2)) `shouldThrow` errorCall "Votre entreprise n'a plus de CEO"
        it "enlever l'employé 3 de l'entreprise 2" $ do
            virerEmploye entreprise2 (fst employe3) `shouldBe` Just entreprise2 {employes = [employe2]}
    describe "changer le poste d'un employé" $ do
        it "Votre entreprise n'a plus de CEO" $ do
            evaluate (changerPosteEmployer entreprise1 (fst employe1) ProductManager) `shouldThrow` errorCall "Votre entreprise n'a plus de CEO"
        it "changer le poste d'un employé" $ do
            changerPosteEmployer entreprise2 (fst employe3) Developer `shouldBe` Just entreprise2 {employes = [(CitId 2,CEO),(CitId 3,Developer)]}
    describe "augmenter le capital de l'entreprise" $ do
        it "augmenter le capital de l'entreprise" $ do
            augmenterCapital entreprise1 1000 `shouldBe` Just entreprise1 {capital = 2000.0}
        it "erreur capital négatif" $ do
            evaluate (augmenterCapital entreprise1 (-1000)) `shouldThrow` errorCall "Vous ne pouvez pas rajoputer un montant négatif"
    describe "diminuer le capital d'une entreprise "$ do
        it "diminuer le capital de l'entreprise" $ do
            diminuerCapital entreprise1 500 `shouldBe` Just entreprise1 {capital = 500.0}
        it "erreur capital négatif" $ do
            evaluate (diminuerCapital entreprise1 2000) `shouldThrow` errorCall "Vous ne pouvez pas avoir un capital négatif"
    describe "enlever un batiment de l'entreprise" $ do
        it "Votre entreprise n'a pas de batiments" $ do
            evaluate (enleverBatiment entreprise1 (BatId 1)) `shouldThrow` errorCall "Votre entreprise n'a pas de batiments"
        it "enlever un batiment de l'entreprise" $ do
            enleverBatiment entreprise2 (BatId 2) `shouldBe` Just entreprise2 {batiments = [BatId 1], employes = [employe2,employe3]}
        it "enlever un batiment de l'entreprise" $ do
            enleverBatiment entreprise2 (BatId 1) `shouldBe` Just entreprise2 {batiments = [BatId 2], employes = [employe2,employe3]}
        it "laisser une entreprise sans batiment" $ do
            evaluate (enleverBatiment entreprise3 (BatId 3)) `shouldThrow` errorCall "Votre entreprise n'a pas de batiments"
    describe "filtrer un batiment de l'entreprise" $ do
        it "filtrer l'entreprise" $ do
            filtrerBatiment entreprise1 (BatId 1) `shouldBe` entreprise1 {batiments=[]}
        it "filtrer avec plusieurs batiments" $ do
            filtrerBatiment entreprise2 (BatId 2) `shouldBe` entreprise2 {batiments = [BatId 1]}
        it "filtrer un batiment de l'entreprise" $ do
            evaluate (filtrerBatiment entreprise3 (BatId 4)) `shouldThrow` errorCall "batiment non trouvé"
    describe "invariant de l'entreprise" $ do
        it "entreprise en faillite" $ do
            evaluate (entreprisePasEnFaillite entreprise5) `shouldThrow` errorCall "Votre entreprise est en faillite"
        it "entreprise sans employés" $ do
            evaluate (entrepriseAyantDesEmployes entreprise6) `shouldThrow` errorCall "Votre entreprise n'a pas d'employés"
        it "entreprise sans batiments" $ do
            evaluate (entepriseAyantDesBatiments entreprise7) `shouldThrow` errorCall "Votre entreprise n'a pas de batiments"
        it "entreprise ayant deux CEO" $ do
            evaluate (entrepriseAyantUnSeulCEO entreprise8) `shouldThrow` errorCall "Votre entreprise doit avoir un seul CEO"
        it "entreprise ayant un seul CEO" $ do
            entrepriseAyantUnSeulCEO entreprise2 `shouldBe` True
        it "entreprise n'ayant aucun CEO" $ do
            evaluate (entrepriseAyantUnSeulCEO entreprise9) `shouldThrow` errorCall "Votre entreprise n'a plus de CEO"
        it "invariant complet " $ do 
            invariantEntreprise entreprise2 `shouldBe` True
    describe "gestion des vehicules d'entreprise " $ do
        it "achat de véhicule d'une autre entreprise" $ do
            acheterVehiculeParEntreprise entreprise2 vehic1 `shouldBe` (entreprise2 {capital=0.0,vehiculesService = [getVehiculeId vehic1]}, vehic1 {propriataire = Just (VehiculeEntreprise (EntrepriseId 2))})
            -- evaluate (acheterVehiculeParEntreprise entreprise vehic1) `shouldThrow` errorCall "Ce vehicule appartient a une entreprise et ne peut pas etre vendu a une entreprise"
        -- it "achat d'un vehicule d'un citoyen" $ do
        --     -- acheterVehiculeParEntreprise entreprise2 vehic2 `shouldBe` (entreprise2 {capital=0.0,vehiculesService = [getVehiculeId vehic2]}, vehic2 {propriataire = Just (VehiculeEntreprise (EntrepriseId 2))})
        --     evaluate (acheterVehiculeParEntreprise entreprise2 vehic2) `shouldThrow` errorCall "Ce vehicule appartient a un citoyen"
        it " achat normal" $ do 
             fst (acheterVehiculeParEntreprise entreprise1 vehic3) `shouldBe` Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 0.0, vehiculesService = [VehicId 3]}