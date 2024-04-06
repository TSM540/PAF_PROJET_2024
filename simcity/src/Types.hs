module Types where 
import Forme
import Occupation
-- ce module est définie pour éviter les dépendances circulaires entre les modules
newtype BatId = BatId Int deriving (Show,Eq, Ord)
newtype ZonId = ZonId Int  deriving (Show,Eq, Ord)
newtype CitId = CitId Int  deriving (Show,Eq, Ord)
newtype PrefId = PrefId Int  deriving (Show,Eq, Ord)

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



data Personne = Personne {
                    idCit :: CitId,
                    coord :: Coord,
                    occupation :: Occupation,
                    crimes :: [String],
                    nationalite :: Nationalite
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
           courses :: Maybe BatId
}deriving (Show,Eq)

