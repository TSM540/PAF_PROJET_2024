module Types where 
import Forme
import Occupation
import Maladies
-- ce module est définie pour éviter les dépendances circulaires entre les modules
newtype BatId = BatId Int deriving (Show,Eq, Ord)
newtype ZonId = ZonId Int  deriving (Show,Eq, Ord)
newtype CitId = CitId Int  deriving (Show,Eq, Ord)
newtype PrefId = PrefId Int  deriving (Show,Eq, Ord)
newtype BankId = BankId Int  deriving (Show,Eq, Ord)
newtype ProdId = ProdId Int  deriving (Show,Eq, Ord)
newtype VehicId = VehicId Int  deriving (Show,Eq, Ord)
newtype EntrepriseId = EntrepriseId Int  deriving (Show,Eq, Ord)
newtype Capacite = Capacite Int deriving (Show, Eq)
newtype ParkingId = ParkingId Int deriving (Show, Eq, Ord)

-- nationalité
data Nationalite = Francais 
                    | Etranger TypeSejour deriving (Show,Eq)

data TypeSejour = 
      Etudiant AnsSejour Diplome
      | Salarie AnsResidence
      deriving (Show,Eq)

data Diplome = Obtenu 
            | EnCours AnneesEtudesPrevu deriving (Show,Eq,Ord)
newtype AnsSejour = AnsSejour Integer deriving (Show,Eq,Ord)
newtype AnsResidence = AnsResidence Integer deriving (Show,Eq,Ord)
newtype AnneesEtudesPrevu = AnneesEtudesPrevu Integer deriving (Show,Eq,Ord)
newtype Pays = Pays String deriving (Show,Eq,Ord)


data Personne = Personne {
                    idCit :: CitId,
                    coord :: Coord,
                    occupation :: Occupation,
                    crimes :: [String],
                    nationalite :: Nationalite,
                    maladies :: [Maladie]
                } deriving (Show,Eq)


data Vie = Vie {
                argentEnBanque:: Float,
                sante :: Integer,
                niveauFaim:: Integer,
                niveauFatigue:: Integer
} deriving (Show,Eq)
data ViePersonnelle = ViePersonnelle {
           maison :: BatId,
           travail :: Maybe BatId,
           courses :: Maybe BatId,
           vehicules :: [VehicId]
}deriving (Show,Eq)
data Entreprise = Entreprise {
    idEntreprise :: EntrepriseId,
    batiments :: [BatId],
    employes :: [(CitId,Poste)],
    capital :: Float,
    vehiculesService :: [VehicId]
} deriving (Show,Eq)

data Poste = 
            CEO 
            | CTO 
            | ProductManager 
            | Developer
            | Designer
            | HR
            | Accountant
            | Sales
            | Marketing
            | Security
            | Cleaning
            | Driver
            deriving (Show,Eq)
-- Parking

data Parking = 
                ParkingMaison  Capacite [VehicId] ParkingId
              | ParkingImmeuble  Capacite [VehicId] ParkingId
                deriving (Eq)
data PropritaireVehicule =    VehiculeEntreprise EntrepriseId
                            | VehiculeCitoyen CitId
                            deriving (Show,Eq)

getEntrepriseId :: PropritaireVehicule -> EntrepriseId
getEntrepriseId (VehiculeEntreprise eid) = eid
getEntrepriseId (VehiculeCitoyen _) = error "Ce vehicule appartient a une entreprise"

getCitoyenId :: PropritaireVehicule -> CitId
getCitoyenId (VehiculeCitoyen cid) = cid
getCitoyenId (VehiculeEntreprise _) = error "Ce vehicule appartient a un citoyen"

