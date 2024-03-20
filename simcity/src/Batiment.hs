module Batiment where
import Forme 
import Citoyen


data Batiment =   Cabane Forme Coord Int [CitId]
                | Atelier Forme Coord Int [CitId]
                | Epicerie Forme Coord Int [CitId]
                | Commissariat Forme Coord
newtype BatId = BatId Int
