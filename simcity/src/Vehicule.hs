module Vehicule where 
import Types

data Vehicule = Vehicule {
    idVehic :: VehicId,
    typeVehic :: TypeVehicule,
    immatriculation :: String,
    propriataire :: Maybe PropritaireVehicule,
    passagers :: [CitId],
    prixVehic :: Float
} deriving (Show, Eq)

data TypeVehicule = Voiture
                    | Moto
                    | Camion
                    | Bus
                    | Helicoptere
                     deriving (Show, Eq)

-- on suppose que si le vehicule appartient a une entreprise, il ne peut pas etre vendu a un citoyen, mais le contraire peut être vrai
acheterVehiculeParCitoyen :: Vehicule -> CitId -> Vehicule
acheterVehiculeParCitoyen v@(Vehicule idv t im prop pas prix) cid = 
        
    case prop of
        Just (VehiculeEntreprise eid) -> error "Ce vehicule appartient a une entreprise et ne peut pas etre vendu a un citoyen"
        Just (VehiculeCitoyen cid') -> 
            if cid == cid' then error "Ce citoyen est deja proprietaire de ce vehicule"
            else
                v {propriataire = Just (VehiculeCitoyen cid)}
        Nothing -> v {propriataire = Just (VehiculeCitoyen cid)}

acheterVehiculeParEntreprise :: Vehicule -> EntrepriseId -> Vehicule
acheterVehiculeParEntreprise v@(Vehicule id t im prop pas prix) eid = 
    case prop of 
        Just (VehiculeEntreprise eid') -> 
            if eid == eid' then error "Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même"
            else
                v {propriataire = Just (VehiculeEntreprise eid)}
        _ -> v {propriataire = Just (VehiculeEntreprise eid)}


vehic1 = Vehicule (VehicId 1) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1))) [] 1000

vehic2 = Vehicule (VehicId 2) Voiture "12534" (Just (VehiculeCitoyen (CitId 1))) [] 1000

-- >>> acheterVehiculeParCitoyen vehic2 (CitId 1)
-- Ce citoyen est deja proprietaire de ce vehicule

-- >>> acheterVehiculeParCitoyen vehic1 (CitId 1)
-- Ce vehicule appartient a une entreprise et ne peut pas etre vendu a un citoyen

-- >>> acheterVehiculeParEntreprise vehic1 (EntrepriseId 1)
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même

achatVehicule :: Vehicule -> PropritaireVehicule -> Vehicule
achatVehicule v@(Vehicule id t im prop pas prix) nouvauProp = 
    case prop of
        Just (VehiculeEntreprise eid) -> acheterVehiculeParEntreprise v (getEntrepriseId nouvauProp)
        Just (VehiculeCitoyen cid) -> acheterVehiculeParCitoyen v (getCitoyenId nouvauProp)
        Nothing -> v {propriataire = Just nouvauProp}
       

-- vehic3 = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1))) [] 1000
vehiCit = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeCitoyen (CitId 1) )) [] 1000
vehicNothing = Vehicule (VehicId 3) Voiture "12534" Nothing [] 1000
vehiEntre = Vehicule (VehicId 3) Voiture "12534" (Just (VehiculeEntreprise (EntrepriseId 1) )) [] 1000

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
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0}

-- >>> nvehicNothing
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0}

-- >>> nvehicNothing'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0}

-- >>> nvehiEntre
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même

-- >>> nvehiEntre'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0}
