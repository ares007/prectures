data Circle = Circle (Int, Int) Float
circles = [Circle (1,1) 5, Circle (3,2) 2.2]

instance Eq Circle where
	Circle (p1x, p1y) r1 == Circle (p2x, p2y) r2 | p1x == p2x && p1y == p2y && r1 == r2 = True
						     | otherwise = False
