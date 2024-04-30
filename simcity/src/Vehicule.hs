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

achatVehicule :: Vehicule -> PropritaireVehicule -> Vehicule
achatVehicule v@(Vehicule id t im prop pas prix c) nouvauProp =
    case prop of
        Just (VehiculeEntreprise eid) -> vehiculeParEntreprise v (getEntrepriseId nouvauProp)
        Just (VehiculeCitoyen cid) -> vehiculeParCitoyen v (getCitoyenId nouvauProp)
        Nothing -> v {propriataire = Just nouvauProp}





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


nombreDePassagersDepassantCapacite :: Vehicule -> Bool
nombreDePassagersDepassantCapacite (Vehicule _ _ _ _ passagers _ c) = length passagers > fromIntegral c

voitureAPropritaire :: Vehicule -> Bool
voitureAPropritaire (Vehicule _ _ _ (Just (VehiculeEntreprise _)) _ _ _) = True
voitureAPropritaire (Vehicule _ _ _ (Just (VehiculeCitoyen _)) _ _ _) = True
voitureAPropritaire ((Vehicule _ _ _ Nothing _ _ _)) = False

sansPassagerDoublons :: Vehicule -> Bool
sansPassagerDoublons (Vehicule _ _ _ _ passagers _ _) = length passagers == length (Prelude.nub passagers)


-- ! rouler une voiture

-- data Vehicule = Vehicule {
--     idVehic :: VehicId,
--     typeVehic :: TypeVehicule,
--     immatriculation :: String,
--     propriataire :: Maybe PropritaireVehicule,
--     passagers :: [CitId],
--     prixVehic :: Float,
--     capacitePassagers :: Integer
-- } deriving (Show, Eq)
roulerCorrectement :: Vehicule -> Bool
roulerCorrectement v@(Vehicule id t im prop pass prix c)
  | not (voitureAPropritaire v) = error "Ce vehicule n'a toujours pas de propritaire, seul le proprietaire peut rouler"
  | null pass = error "Le vehicule ne peut pas rouler sans passagers"
  | t == Helicoptere = error "Un helicoptere ne peut pas rouler"
  | otherwise = True

