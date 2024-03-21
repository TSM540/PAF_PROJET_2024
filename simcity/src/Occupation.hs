module Occupation where 
import Forme 
data Occupation = 
        Travailler 
        | Dormir 
        | FaireCourses
        | SeDeplacer Coord 

-- instance de Occupation
instance Show Occupation where
    show Travailler = "Travailler"
    show Dormir = "Dormir"
    show FaireCourses = "Faire des courses"
    show (SeDeplacer coord) = "Se déplacer vers " ++ show coord