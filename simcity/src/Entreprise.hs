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

