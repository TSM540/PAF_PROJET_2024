module Forme where 

-- Constructeurs
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
    deriving (Show, Eq)

-- Fonctions 
limites :: Forme -> (Int,Int,Int,Int)
limites (HSegement (C x y) l) = (x, x+l, y, y) -- Point le plus a gauche, Point le plus a droite, Point le plus en haut, Point le plus en bas
limites (VSegement (C x y) l) = (x, x, y, y+l)
limites (Rectangle (C x y) w h) = (x, x+w, y, y+h)

forme1 = HSegement (C 1 2) 5
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
    (cy == y) && ((cx >= x) || (cx <= x+l)) -- point entre le point le plus a gauche et le point le plus a droite
adjacent (C cx cy) (VSegement (C x y) l) =
    (cx == x) && ((cy >= y) || (cy == y+l)) -- point entre le point le plus en haut et le point le plus en bas
adjacent (C cx cy) (Rectangle (C x y) w h) =
    (cx == x) && ((cy > y) && (cy < y+h)) -- point entre le point le plus en haut et le point le plus en bas, mais pas sur les coins
    || (cx == x+w) && ((cy > y) && (cy < y+h)) -- point le plus à droit du rectangle et entre le point le plus en haut et le point le plus en bas, mais pas sur les coins
    || (cy == y) && ((cx > x) && (cx < x+w)) -- point entre le point le plus a gauche et le point le plus a droite du rectangle a la position Nord, mais pas sur les coins
    || (cy == y+h) && ((cx > x) && (cx < x+w)) -- point entre le point le plus a gauche et le point le plus a droite du rectangle a la position Sud, mais pas sur les coins




point1 = C (-1) 1
segment1 = HSegement (C 0 1) 5
-- >>> adjacent point1 segment1
-- True

point2 = C 0 0
segment2 = VSegement (C 0 1) 5
-- >>> adjacent point2 segment2
-- False

point3 = C 2 5
rectangle1 = Rectangle (C 0 0) 5 5
-- >>> adjacent point3 rectangle1
-- True


-- ! la fonction proche renvoi su un point est il est proche d'une forme (a une distance de 1)
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

-- ! récuperer les 
getW (a,_,_,_) = a
getE (_,b,_,_) = b
getN (_,_,c,_) = c
getS (_,_,_,d) = d

collision_approx :: Forme -> Forme -> Bool
collision_approx f1 f2 = 
    (getE (limites f1) <= getW (limites f2)) && (getE (limites f1) >= getW (limites f2)) -- si les limites de l'axe X de f1 sont en collision avec les limites de l'axe X de f2
    && (getN (limites f1) <= getS (limites f2)) && (getS (limites f1) >= getN (limites f2)) -- si les limites de l'axe Y de f1 sont en collision avec les limites de l'axe Y de f2
    -- on retourne les faux positifs si les coordonnées W E N S de f1 sont proche de f2
  || (proche (C (getW (limites f1)) (getN (limites f1))) f2)
  || (proche (C (getW (limites f1)) (getS (limites f1))) f2)
  || (proche (C (getE (limites f1)) (getN (limites f1))) f2)
  || (proche (C (getE (limites f1)) (getS (limites f1))) f2)

f1 = Rectangle (C 0 0) 10 5
f2 = Rectangle (C 5 5) 10 5

f3 = Rectangle (C 0 0) 2 2
f4 = Rectangle (C 4 4) 2 2

f5 = Rectangle (C 0 0) 2 2
f6 = Rectangle (C 2 2) 2 2

-- >>> collision_approx f1 f2
-- True
-- >>> collision_approx f3 f4
-- False
-- >>> collision_approx f5 f6
-- True
collision :: Forme -> Forme -> Bool
collision f1 f2 =
    -- Vérifier si les formes se chevauchent
    ((appartient (C (getW (limites f1)) (getN (limites f1))) f2)
    || (appartient (C (getW (limites f1)) (getS (limites f1))) f2)
    || (appartient (C (getE (limites f1)) (getN (limites f1))) f2)
    || (appartient (C (getE (limites f1)) (getS (limites f1))) f2))
    -- Vérifier si les formes sont adjacentes
    && adjacentes f1 f2





adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 =
  (adjacent (C (getW (limites f1)) (getN (limites f1))) f2)
  || (adjacent (C (getW (limites f1)) (getS (limites f1))) f2)
  || (adjacent (C (getE (limites f1)) (getN (limites f1))) f2)
  || (adjacent (C (getE (limites f1)) (getS (limites f1))) f2)




-- >>> collison f1 f2
