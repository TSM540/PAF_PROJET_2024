module Types where 

-- ce module est définie pour éviter les dépendances circulaires entre les modules
newtype BatId = BatId Int deriving (Show,Eq, Ord)
newtype ZonId = ZonId Int  deriving (Show,Eq, Ord)
newtype CitId = CitId Int  deriving (Show,Eq, Ord)



