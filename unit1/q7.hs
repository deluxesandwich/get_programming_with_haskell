-- 1. Identify the end goal(s).
-- 2. Determine what happens when a goal is reached.
-- 2. Determine what happens when a goal is reached.
-- 3. List all alternate possibilities.
-- 4. Determine your “rinse and repeat” process.
-- 5. Ensure that each alternative moves you toward the goal.
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
