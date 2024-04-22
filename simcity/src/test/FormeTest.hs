module FormeTest where

import Forme 
-- src
    -- Forme.hs 
    -- test
        -- FormeTest.hs
        -- test.hs 

-- >>> C 1 2
-- C {cx = 1, cy = 2}

-- E : East (Est)
-- W : West (Ouest)
-- N : North (Nord)
-- S : South (Sud)
-- E W N S


forme1 = HSegement (C 1 2) 5
forme2 = VSegement (C 1 2) 5
forme3 = Rectangle (C 1 2) 5 5
-- >>> limites forme1 -- (1, 6, 2, 2)
-- (1,6,2,2)

-- >>> limites forme2 
-- (1,1,2,7)

-- >>> limites forme3 
-- (1,6,2,7)


-- >>> appartient (C 3 2) forme1
-- True
-- >>> appartient (C 3 2) forme2
-- False
-- >>> appartient (C 3 2) forme3
-- True



point1 = C (-1) 1
segment1 = HSegement (C 0 1) 5
-- >>> adjacent point1 segment1
-- False

point2 = C 0 0
segment2 = VSegement (C 0 1) 5
-- >>> adjacent point2 segment2
-- False

point3 = C 2 5
rectangle1 = Rectangle (C 0 0) 5 5
-- >>> adjacent point3 rectangle1
-- True

-- >>> adjacent (C 0 0) (Rectangle (C 0 0) 5 5)
-- False

f1 = Rectangle (C 0 0) 10 5
f2 = Rectangle (C 5 5) 10 5

f3 = Rectangle (C 0 0) 2 2
f4 = Rectangle (C 4 4) 2 2

f5 = Rectangle (C 0 0) 2 2
f6 = Rectangle (C 2 2) 2 2

-- >>> collisionApprox f1 f2
-- True
-- >>> collisionApprox f3 f4
-- False
-- >>> collisionApprox f5 f6
-- True

-- >>> collision f1 f2
-- True
