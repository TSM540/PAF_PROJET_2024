module ZoneTest where
import Zone
import Forme

-- >>> zoneForme (Eau (Rectangle (C 0 0) 10 5))
-- Rectangle (C {cx = 0, cy = 0}) 10 5


-- z1 dans z2
zonesDisjointesTest1 = zonesDisjointes (Eau (Rectangle (C 0 0) 1 2)) (Route (Rectangle (C 0 0) 10 5))

-- >>> zonesDisjointesTest1
-- False

-- adjacantes

zonesDisjointesTest2 = zonesDisjointes (Eau (Rectangle (C 0 0) 1 2)) (Eau (Rectangle (C 1 2) 10 5))

-- >>> zonesDisjointesTest2
-- False

zonesDisjointesTest3 = zonesDisjointes (Eau (Rectangle (C 0 0) 1 2)) (Eau (Rectangle (C 2 3) 10 5))

-- >>> zonesDisjointesTest3
-- True
