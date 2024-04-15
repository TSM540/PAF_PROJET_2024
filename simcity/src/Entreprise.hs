module Entreprise where
import Types
import Forme
import Vehicule

-- changement sur l'entreprise
recruterEmploye :: Entreprise ->CitId-> Poste  -> Maybe Entreprise
recruterEmploye (Entreprise eid  batiments employes capital lv) cid p =
    let ne = Entreprise eid  batiments ((cid,p):employes) capital lv in
    if invariantEntreprise ne then  Just ne
    else Nothing


virerEmploye :: Entreprise -> CitId -> Maybe Entreprise
virerEmploye (Entreprise eid  batiments employes capital lv) cid =
    let ne =Entreprise eid  batiments (filter (\(cid',_) -> cid /= cid') employes) capital lv in
    if invariantEntreprise ne then Just ne
                         else   Nothing


changerPosteEmployer :: Entreprise -> CitId -> Poste -> Maybe Entreprise
changerPosteEmployer (Entreprise eid  batiments employes capital lv) cid p =
    let ne = Entreprise eid  batiments (map (\(cid',p') -> if cid == cid' then  (cid,p) else (cid',p')) employes) capital lv in
        if invariantEntreprise ne then Just ne
                            else Nothing

augmenterCapital :: Entreprise -> Float -> Maybe Entreprise
augmenterCapital (Entreprise eid  batiments employes capital lv) montant =
     let ne = Entreprise eid  batiments employes (capital + montant) lv in
        if invariantEntreprise ne then Just ne
                           else  Nothing

diminuerCapital :: Entreprise -> Float -> Maybe Entreprise
diminuerCapital e@(Entreprise eid  batiments employes capital lv) montant =
    if (capital - montant) < 0 then error "Vous ne pouvez pas avoir un capital négatif"
    else
        let ne = Entreprise eid  batiments employes (capital - montant) lv in
            if invariantEntreprise ne then Just ne
            else Nothing

enleverBatiment :: Entreprise -> BatId -> Maybe Entreprise
enleverBatiment e bid =
                let ne = filtrerBatiment e bid
                in
                case ne of
                    (Entreprise _  bat' _ 0 _) ->
                            if null bat' then error "Vous ne pouvez pas avoir une entreprise sans batiment"
                            else
                                 if invariantEntreprise ne then Just ne
                                else  Nothing
                    _ -> if invariantEntreprise ne then Just ne
                        else Nothing


filtrerBatiment :: Entreprise -> BatId -> Entreprise
filtrerBatiment (Entreprise eid  batiments employes capital lv) bid =
        Entreprise eid  (filter (bid /=) batiments) employes capital lv


-- invariant de l'entreprise
invariantEntreprise :: Entreprise -> Bool
invariantEntreprise e =
    entreprisePasEnFaillite e
    && entrepriseAyantDesEmployes e
    && entepriseAyantDesBatiments e
    && entrepriseAyantUnSeulCEO e

entreprisePasEnFaillite :: Entreprise -> Bool
entreprisePasEnFaillite (Entreprise eid  batiments employes capital _) = (capital > 0) || error "Votre entreprise est en faillite"

entrepriseAyantDesEmployes :: Entreprise -> Bool
entrepriseAyantDesEmployes (Entreprise _  _ employes _ _) = not (null employes) || error "Votre entreprise n'a pas d'employés"

entepriseAyantDesBatiments :: Entreprise -> Bool
entepriseAyantDesBatiments (Entreprise _  batiments _ _ _) = not (null batiments) || error "Votre entreprise n'a pas de batiments"

entrepriseAyantUnSeulCEO :: Entreprise -> Bool
entrepriseAyantUnSeulCEO (Entreprise _  _ employes _ _) = length (filter (\(_,p) -> p == CEO) employes) == 1 ||
                                                        if not (any (\(_,p) -> p == CEO) employes) then error "Votre entreprise n'a plus de CEO"
                                                        else           error "Votre entreprise doit avoir un seul CEO"

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
-- Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 3,ProductManager),(CitId 1,CEO)], capital = 1000.0})


-- >>> virerEmploye entreprise2 (fst employe2)
-- Votre entreprise est en faillite

-- >>> virerEmploye entreprise1 (fst employe1)
-- Votre entreprise n'a pas d'employés

-- >>> virerEmploye entreprise1 (fst employe2)
-- Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 1000.0})


-- >>> changerPosteEmployer entreprise1 (fst employe1) ProductManager
-- Votre entreprise n'a pas de CEO

-- >>> changerPosteEmployer entreprise1 (fst employe2) ProductManager
-- Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 1000.0})

-- >>> show (augmenterCapital entreprise1 1000)
-- "Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 2000.0})"

-- >>> show (diminuerCapital entreprise1 500)
-- "Just (Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 500.0})"


-- >>> show (enleverBatiment entreprise1 (BatId 1))
-- Votre entreprise n'a pas de batiments

-- >>> show (enleverBatiment entreprise2 (BatId 2))
-- "Just (Entreprise {idEntreprise = EntrepriseId 2, batiments = [BatId 1], employes = [(CitId 2,CEO)], capital = 1000.0})"

-- >>> show (enleverBatiment entreprise2 (BatId 1))
-- "Just (Entreprise {idEntreprise = EntrepriseId 2, batiments = [BatId 2], employes = [(CitId 2,CEO)], capital = 1000.0})"

acheterVehiculeParEntreprise :: Entreprise -> Vehicule -> (Entreprise, Vehicule)
acheterVehiculeParEntreprise (Entreprise id bat empl capital lv) 
            vehicule@(Vehicule idv typev imm prop pass prix cap) = 
               let nv = achatVehicule vehicule (VehiculeEntreprise id) in
                if capital - prix < 0 then error "L'entreprise n'a pas assez de capital pour acheter ce véhicule"
                else ( 
                      Entreprise id bat empl (capital - prix) (getVehiculeId nv : lv)
                    ,
                    nv
                )

-- >>> acheterVehiculeParEntreprise entreprise1 vehic1
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même
                  
-- >>> acheterVehiculeParEntreprise entreprise1 vehic2
-- Ce vehicule appartient a un citoyen

-- >>> fst (acheterVehiculeParEntreprise entreprise1 vehic3)
-- Entreprise {idEntreprise = EntrepriseId 1, batiments = [BatId 1], employes = [(CitId 1,CEO)], capital = 0.0, vehiculesService = [VehicId 3]}
