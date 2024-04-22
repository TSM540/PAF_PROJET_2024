
module EntrepriseTest where
import Entreprise 
import Types
import Forme
import Vehicule

-- test
entreprise1 = Entreprise (EntrepriseId 1)  [BatId 1] [employe1] 1000 []

entreprise2 = Entreprise (EntrepriseId 2)  [BatId 2,BatId 1] [(fst employe2,CEO)] 1000 []

entreprise3 = Entreprise (EntrepriseId 3)  [BatId 3] [employe1] 1000 []

entreprise4 = Entreprise (EntrepriseId 4)  [BatId 4] [employe1] 1000 []

employe1 = (CitId 1,CEO)
employe2 = (CitId 2,CEO)
employe3 = (CitId 3,ProductManager)
employe4 = (CitId 4,CTO)


nefalse = uncurry (recruterEmploye entreprise1) employe2
netrue = uncurry (recruterEmploye entreprise1) employe3
-- >>> recruterEmploye entreprise1 (fst employe2) (snd employe2) 
-- Votre entreprise doit avoir un seul CEO

-- >>> netrue
-- Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 3,ProductManager),(CitId 1,CEO)], capital = 1000.0, vehiculesService = []})


-- >>> virerEmploye entreprise2 (fst employe2)
-- Votre entreprise n'a pas d'employés

-- >>> virerEmploye entreprise1 (fst employe1)
-- Votre entreprise n'a pas d'employés

-- >>> virerEmploye entreprise1 (fst employe2)
-- Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 1000.0, vehiculesService = []})


-- >>> changerPosteEmployer entreprise1 (fst employe1) ProductManager
-- Votre entreprise n'a plus de CEO

-- >>> changerPosteEmployer entreprise1 (fst employe2) ProductManager
-- Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 1000.0, vehiculesService = []})

-- >>> show (augmenterCapital entreprise1 1000)
-- "Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 2000.0, vehiculesService = []})"

-- >>> show (diminuerCapital entreprise1 500)
-- "Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 500.0, vehiculesService = []})"


-- >>> show (enleverBatiment entreprise1 (BatId 1))
-- Votre entreprise n'a pas de batiments

-- >>> show (enleverBatiment entreprise2 (BatId 2))
-- "Just (Entreprise {idEntreprise = EntrepriseId 2, batiments = [BatId 1], employes = [(CitId 2,CEO)], capital = 1000.0, vehiculesService = []})"

-- >>> show (enleverBatiment entreprise2 (BatId 1))
-- "Just (Entreprise {idEntreprise = EntrepriseId 2, batiments = [BatId 2], employes = [(CitId 2,CEO)], capital = 1000.0, vehiculesService = []})"

-- >>> acheterVehiculeParEntreprise entreprise1 vehic1
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même
                  
-- >>> acheterVehiculeParEntreprise entreprise1 vehic2
-- Ce vehicule appartient a un citoyen

-- >>> fst (acheterVehiculeParEntreprise entreprise1 vehic3)
-- Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 0.0, vehiculesService = [VehicId 3]}
