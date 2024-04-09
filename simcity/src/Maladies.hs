module Maladies where 


data Maladie = 
    Maladie {
        nomMaladie :: String,
        symptomes :: [String],
        traitement :: String,
        typeMaladie :: TypeMaladie
    } deriving (Show, Eq)

data TypeMaladie = 
    Infectieuse 
    | Chronique 
    | Mortelle
    deriving (Show, Eq)