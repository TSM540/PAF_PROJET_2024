module Types where 
import Forme
import Occupation
-- ce module est définie pour éviter les dépendances circulaires entre les modules
newtype BatId = BatId Int deriving (Show,Eq, Ord)
newtype ZonId = ZonId Int  deriving (Show,Eq, Ord)
newtype CitId = CitId Int  deriving (Show,Eq, Ord)


-- nationalité
data Nationalite = Francais 
                    | Etranger TypeSejour deriving (Show)

data TypeSejour = 
      Etudiant AnsSejour Diplome
      | Salarie AnsResidence
      deriving (Show)

data Diplome = Obtenu 
            | EnCours AnneesEtudesPrevu deriving (Show)
newtype AnsSejour = AnsSejour Int deriving (Show)
newtype AnsResidence = AnsResidence Int deriving (Show)
newtype AnneesEtudesPrevu = AnneesEtudesPrevu Int deriving (Show)

data Personne = Personne {
                    coord :: Coord,
                    occupation :: Occupation,
                    crimes :: [String],
                    nationalite :: Nationalite
                } deriving (Show)


data Vie = Vie {
                argentEnBanque:: Float,
                sante :: Int,
                niveauFaim:: Int
} deriving (Show)
data ViePersonnelle = ViePersonnelle {
           maison :: BatId,
           travail :: Maybe BatId,
           courses :: Maybe BatId
}deriving (Show)

