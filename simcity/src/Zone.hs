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

newtype ZonId = ZonId Int 

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



