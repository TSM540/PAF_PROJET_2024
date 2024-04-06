module Occupation where 
import Forme 
data Occupation = 
        Travailler Float -- Salaire journalier
        | Dormir Int -- nombre d'heures
        | FaireCourses Float -- Prix des courses 
        | SeDeplacer Coord 
        | ALaMaison
        | Manger 
        | Cuisiner 
        deriving(Eq)

-- instance de Occupation
instance Show Occupation where
  show (Travailler s) = "Travailler avec " ++ show s ++ "€ de salaire journalier"
  show (Dormir h) = "Dormir " ++ show h ++ " heures"
  show (FaireCourses c) = "Faire des courses d'un montant de " ++ show c ++ "€"
  show (SeDeplacer coord) = "Se déplacer vers " ++ show coord


sommeJournaliere :: Occupation -> Float
sommeJournaliere (Travailler s) = s