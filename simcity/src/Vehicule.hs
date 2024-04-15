module Vehicule where 
import Types
import qualified Distribution.Compat.Prelude as Prelude

data Vehicule = Vehicule {
    idVehic :: VehicId,
    typeVehic :: TypeVehicule,
    immatriculation :: String,
    propriataire :: Maybe PropritaireVehicule,
    passagers :: [CitId],
    prixVehic :: Float,
    capacitePassagers :: Integer
} deriving (Show, Eq)

data TypeVehicule = Voiture
                    | Moto
                    | Camion
                    | Bus
                    | Helicoptere
                     deriving (Show, Eq)

-- on suppose que si le vehicule appartient a une entreprise, il ne peut pas etre vendu a un citoyen, mais le contraire peut être vrai
vehiculeParCitoyen :: Vehicule -> CitId -> Vehicule
vehiculeParCitoyen v@(Vehicule idv t im prop pas prix c) cid = 
        
    case prop of
        Just (VehiculeEntreprise eid) -> error "Ce vehicule appartient a une entreprise et ne peut pas etre vendu a un citoyen"
        Just (VehiculeCitoyen cid') -> 
            if cid == cid' then error "Ce citoyen est deja proprietaire de ce vehicule"
            else
                v {propriataire = Just (VehiculeCitoyen cid)}
        Nothing -> v {propriataire = Just (VehiculeCitoyen cid)}

vehiculeParEntreprise :: Vehicule -> EntrepriseId -> Vehicule
vehiculeParEntreprise v@(Vehicule id t im prop pas prix c) eid = 
    case prop of 
        Just (VehiculeEntreprise eid') -> 
            if eid == eid' then error "Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même"
            else
                v {propriataire = Just (VehiculeEntreprise eid)}
        _ -> v {propriataire = Just (VehiculeEntreprise eid)}

getVehiculeId :: Vehicule -> VehicId
getVehiculeId (Vehicule id _ _ _ _ _ _) = id
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

achatVehicule :: Vehicule -> PropritaireVehicule -> Vehicule
achatVehicule v@(Vehicule id t im prop pas prix c) nouvauProp = 
    case prop of
        Just (VehiculeEntreprise eid) -> vehiculeParEntreprise v (getEntrepriseId nouvauProp)
        Just (VehiculeCitoyen cid) -> vehiculeParCitoyen v (getCitoyenId nouvauProp)
        Nothing -> v {propriataire = Just nouvauProp}
       

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
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0}

-- >>> nvehicNothing
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeCitoyen (CitId 2)), passagers = [], prixVehic = 1000.0}

-- >>> nvehicNothing'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0}

-- >>> nvehiEntre
-- Ce vehicule appartient deja a cette entreprise et ne peut pas être revendu a elle même

-- >>> nvehiEntre'
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [], prixVehic = 1000.0}

-- ! rajouter des passagers a un vehicule
ajouterPassagers :: Vehicule -> [CitId] -> Vehicule
ajouterPassagers (Vehicule id t im prop pass prix c) passagers = 
      let nv = Vehicule id t im prop (pass ++ passagers) prix c in
        if not (voitureAPropritaire nv) 
            then error "Ce vehicule n'a toujours pas de propritaire, seul le proprietaire peut ajouter des passagers"
        else
            if nombreDePassagersDepassantCapacite nv 
                then error "Le nombre de passagers depasse la capacite du vehicule"
            else 
                if not (sansPassagerDoublons nv) 
                    then error "Un passager ne peut pas etre ajouté deux fois"
                else 
                 nv 


-- >>> ajouterPassagers vehic4 [CitId 2]
-- Ce vehicule n'a toujours pas de propritaire, seul le proprietaire peut ajouter des passagers

-- >>> ajouterPassagers vehic3 [CitId 2, CitId 3, CitId 4, CitId 5, CitId 6,CitId 8,CitId 7]
-- Le nombre de passagers depasse la capacite du vehicule

-- >>> ajouterPassagers vehic3 [CitId 2, CitId 3]
-- Vehicule {idVehic = VehicId 3, typeVehic = Voiture, immatriculation = "12534", propriataire = Just (VehiculeEntreprise (EntrepriseId 2)), passagers = [CitId 2,CitId 3], prixVehic = 1000.0, capacitePassagers = 5}

-- >>> ajouterPassagers vehic3 [CitId 3, CitId 3] 
-- Un passager ne peut pas etre ajouté deux fois

nombreDePassagersDepassantCapacite :: Vehicule -> Bool 
nombreDePassagersDepassantCapacite (Vehicule _ _ _ _ passagers _ c) = length passagers > fromIntegral c

voitureAPropritaire :: Vehicule -> Bool 
voitureAPropritaire (Vehicule _ _ _ (Just (VehiculeEntreprise _)) _ _ _) = True
voitureAPropritaire (Vehicule _ _ _ (Just (VehiculeCitoyen _)) _ _ _) = True
voitureAPropritaire _ = False

sansPassagerDoublons :: Vehicule -> Bool
sansPassagerDoublons (Vehicule _ _ _ _ passagers _ _) = length passagers == length (Prelude.nub passagers)


-- ! rouler une voiture
