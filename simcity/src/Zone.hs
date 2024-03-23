module Zone where

import Forme
import Data.Map as Map
import Batiment

data Zone =
    Eau Forme 
    | Route Forme
    | ZoneResidentielle Forme [Batiment]
    | ZoneIndustrielle Forme [Batiment]
    | ZoneCommerciale Forme [Batiment]
    | Admin Forme Batiment
--instance de Zone 

instance Show Zone where
    show (Eau forme) = "Eau " ++ show forme
    show (Route forme) = "Route " ++ show forme
    show (ZoneResidentielle forme batiments) = "Zone résidentielle " ++ show forme ++ " avec " ++ show (length batiments) ++ " bâtiments"
    show (ZoneIndustrielle forme batiments) = "Zone industrielle " ++ show forme ++ " avec " ++ show (length batiments) ++ " bâtiments"
    show (ZoneCommerciale forme batiments) = "Zone commerciale " ++ show forme ++ " avec " ++ show (length batiments) ++ " bâtiments"
    show (Admin forme batiment) = "Zone administrative " ++ show forme ++ " avec le bâtiment " ++ show batiment

 
-- Fonction
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZoneResidentielle f _) = f
zoneForme (ZoneIndustrielle f _) = f
zoneForme (ZoneCommerciale f _) = f
zoneForme (Admin f _) = f



-- >>> zoneForme (Eau (Rectangle (C 0 0) 10 5))
-- Rectangle (C {cx = 0, cy = 0}) 10 5

zonesDisjointes :: Zone -> Zone -> Bool
zonesDisjointes z1 z2 =
    not (collision  (zoneForme z1) (zoneForme z2))


