module Forme where 


data  Coord = C{
    cx :: Int ,
    cy :: Int
} deriving (Show, Eq)

-- >>> C 1 2

-- E : East (Est)
-- W : West (Ouest)
-- N : North (Nord)
-- S : South (Sud)
-- E W N S

data Forme = 
        HSegement Coord Int -- Point le plus à W et la longueur du segement
        | VSegement Coord Int -- Point le plus en N et la longueur du segement
        | Rectangle Coord Int Int -- Point le plus au NW et la largeur(W to E) et la hauteur (N to S)

limites :: Forme -> (Int,Int,Int,Int)
limites (HSegement (C x y) l) = (x, x+l, y, y) -- Point le plus a gauche, Point le plus a droite, Point le plus en haut, Point le plus en bas
limites (VSegement (C x y) l) = (x, x, y, y+l)
limites (Rectangle (C x y) w h) = (x, x+w, y, y+h)

-- forme1 = HSegement (C 1 2) 5
-- >>> limites forme1 -- (1, 6, 2, 2)
-- forme2 = VSegement (C 3 4) 3
-- >>> limites forme2 
-- (3, 3, 4, 7)
-- forme3 = Rectangle (C 0 0) 10 5
-- >>> limites forme3 
-- (0, 10, 0, 5)

appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegement (C x' y') l) = (y == y') && (x >= x') && (x <= x+l)
appartient (C x y) (VSegement (C x' y') l) = (x == x') &&(y >= y') && (y <= y+l)
appartient (C x y) (Rectangle (C x' y') w h) =(x >= x') && (x <= x+w) && (y >= y') &&(y <= y+h)

-- >>> appartient (C 3 2) forme1
-- True
-- >>> appartient (C 3 2) forme2
-- False
-- >>> appartient (C 3 2) forme3
-- True






adjacent ::Coord -> Forme -> Bool
adjacent (C cx cy) (HSegement (C x y) l) =
    (cy==y) && ((cx==x) || (cx==x+l))
adjacent (C cx cy) (VSegement (C x y) l) =
    (cx==x) && ((cy==y) || (cy==y+l))
adjacent (C cx cy) (Rectangle (C x y) w h) =
  -- to continue






proche :: Coord -> Forme -> Bool
proche (C cx cy) (HSegement (C x y) l) =
--   (y == y') && ((x == x'-1) || (x == x'+l))
    ((cy==y) && (cx==x-1)) -- point a gauche du segement
    || ((cy==y) && (cx==x+l+1)) -- point a droite du segement
    || ((cy==y-1) && (cx>=x) && (cx<=x+l)) -- point au dessus du segement
    || ((cy==y+1) && (cx>=x) && (cx<=x+l)) -- point en dessous du segement
    -- si il est a Haut Gauche
    || ((cx==x-1) && (cy==y-1)) -- point en Haut Gauche
    -- si il est a Haut Droite
    || ((cx==x+l+1) && (cy==y-1)) -- point en Haut Droite
    -- si il est a Bas Gauche
    || ((cx==x-1) && (cy==y+1)) -- point en Bas Gauche
    -- si il est a Bas Droite
    || ((cx==x+l+1) && (cy==y+1)) -- point en Bas Droite
    
proche (C cx cy) (VSegement (C x y) l) =
      ((cx==x) && (cy==y-1)) -- point au dessus du segement
    || ((cx==x) && (cy==y+l+1)) -- point en dessous du segement
    || ((cx==x-1) && (cy>=y) && (cy<=y+l)) -- point a gauche du segement
    || ((cx==x+1) && (cy>=y) && (cy<=y+l)) -- point a droite du segement
    -- si il est a Haut Gauche
    || ((cx==x-1) && (cy==y-1)) -- point en Haut Gauche
    -- si il est a Haut Droite
    || ((cx==x+1) && (cy==y-1)) -- point en Haut Droite
    -- si il est a Bas Gauche
    || ((cx==x-1) && (cy==y+l+1)) -- point en Bas Gauche
    -- si il est a Bas Droite
    || ((cx==x+1) && (cy==y+l+1)) -- point en Bas Droite

proche (C cx cy) (Rectangle (C x y) w h) =
    (cx==x-1) && (cy>=y) && (cy<=y+h) -- point a gauche du rectangle
    || (cx==x+w+1) && (cy>=y) && (cy<=y+h) -- point a droite du rectangle
    || (cy==y-1) && (cx>=x) && (cx<=x+w) -- point au dessus du rectangle
    || (cy==y+h+1) && (cx>=x) && (cx<=x+w) -- point en dessous du rectangle
    -- si il est a Haut Gauche
    || ((cx==x-1) && (cy==y-1)) -- point en Haut Gauche
    -- si il est a Haut Droite
    || ((cx==x+w+1) && (cy==y-1)) -- point en Haut Droite
    -- si il est a Bas Gauche
    || ((cx==x-1) && (cy==y+h+1)) -- point en Bas Gauche
    -- si il est a Bas Droite
    || ((cx==x+w+1) && (cy==y+h+1)) -- point en Bas Droite

collision_approx :: Forme -> Forme -> Bool
collision_approx f1 f2 =
  let (W1, E1, N1, S1) = limites f1
      
  in
    (proche W1 f2) && (proche E1 f2) && (proche N1 f2) && (proche S1 f2)
    
-- collision_approx forme1 forme2 -- True



     
