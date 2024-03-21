module Citoyen where 
import Forme 
import Occupation
import Types
data Citoyen = Immigrant Coord (Int , Int , Int) Occupation --()
                | Habitant Coord (Int , Int , Int) (BatId , Maybe BatId , Maybe BatId) Occupation
                | Emigrant Coord Occupation

-- instance Citoyen 

instance Show Citoyen where
    show (Immigrant coord stats occ) = "Immigrant en " ++ show coord ++ " avec stats " ++ show stats ++ " et occupation " ++ show occ
    show (Habitant coord stats habitation occ) = "Habitant en " ++ show coord ++ " avec stats " ++ show stats ++ " et habitation " ++ show habitation ++ " et occupation " ++ show occ
    show (Emigrant coord occ) = "Émigrant en " ++ show coord ++ " avec occupation " ++ show occ