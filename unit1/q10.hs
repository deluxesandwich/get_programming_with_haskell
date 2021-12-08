-- Used in Book:
-- robot (name, attack, hp) = \message -> message (name, attack, hp)
-- Compiler suggested:
robot (name, attack, hp) message = message (name, attack, hp)

name (n, _, _) = n

getName aRobot = aRobot name

attack (_, a, _) = a

getAttack aRobot = aRobot attack

hp (_, _, hp) = hp

getHP aRobot = aRobot hp

setName aRobot newName =
  aRobot (\(_, attack, hp) -> robot (newName, attack, hp))

setAttack aRobot newAttack =
  aRobot (\(name, _, hp) -> robot (name, newAttack, hp))

setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

printRobot aRobot =
  aRobot (\(n, a, h) -> n ++ " attack:" ++ show a ++ " hp:" ++ show h)

damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

killerRobot = robot ("Kill3r", 25, 200)

fastRobot = robot ("speedy", 15, 40)

slowRobot = robot ("slowpoke", 20, 30)

robotSeq = [gentleGiant, killerRobot, fastRobot, slowRobot]

-- Use map on a list of robot objects to get the life of each robot in the list.
getRobotHPs = map getHP

-- Write a threeRoundFight function that takes two robots and has them fight for three
-- rounds, returning the winner. To avoid having so many different variables for robot
-- state, use a series of nested lambda functions so you can just overwrite robotA and
-- robotB.
-- threeRoundFight robotA robotB = fight (fight robotA robotB) robotA
-- TODO: write in nested lambda function one day
testFight = fight killerRobot

-- Create a list of three robots. Then create a fourth robot.
-- Use partial application to create a closure for the fight
-- method so the fourth robot can fight all three robots at once,
-- using map. Finally, use map to get the remaining life from the
-- rest of the robots.
robotList = [fastRobot, slowRobot, gentleGiant]

killerFight = fight killerRobot

robotListAfterFight = map killerFight robotList
-- ghci> map getHP robotListAfterFight
-- [15,5,275]
