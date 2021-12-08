doubleDouble x = dubs * 2
  where
    dubs = x * 2

doubleDoubleLambda x = (\x -> x * 2) x * 2

overwrite x =
  let x = 2
   in let x = 3
       in let x = 4
           in x

overwriteLambda x = (\x -> 4) ((\x -> 3) ((\x -> 2) x))

-- overwrite x = (\x ->
--               (\x ->
--                 (\x -> x) 4
--                )3
--               )2
-- overwrite x = (\x -> (\x -> (\x -> x) 4)3)2
-- counter x = let x = x + 1
--             in
--              let x = x + 1
--              in
--               x
-- counter x = (\x -> (\x -> x) x + 1) x + 1
-- counter x = (\x -> x + 1)
--             ((\x -> x + 1)
--              ((\x -> x) x))
counter x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))
