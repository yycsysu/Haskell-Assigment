-- Author wu

import Data.List

-- a person is Male or Female, Abdicated, Dead or Alive, and has a name
data Sex = Male | Female deriving (Eq, Show)
data Status = Alive | Dead | Abdicated deriving (Eq, Show)
data Person = Person Sex Status String deriving (Eq, Show)
-- a Dynasty is headed by a Person and indicates the descendants
-- oldest first; a Dull Person doesn’t have any recorded descendants
data Dynasty = Descend Person [Dynasty] | Dull Person deriving (Eq, Show)


successors :: String -> Dynasty -> [String]
successors name dy = aliveafter name (linefrom dy)

linefrom :: Dynasty -> [Person]
linefrom dy =
    case reorder dy of
        Descend (Person _ Abdicated _) ds -> []
        Descend person ds -> [person] ++ (concatMap linefrom) ds
        Dull (Person _ Abdicated _) -> []
        Dull person -> [person]

reorder :: Dynasty -> Dynasty
reorder (Dull person) = (Dull person)
reorder (Descend person dys) = (Descend person (sortds $ map reorder dys))

sortds :: [Dynasty] -> [Dynasty]
sortds dys = ms ++ fms
       where (ms, fms) = partition (isMale) dys
             isMale (Descend (Person Male _ _) _) = True
             isMale (Dull (Person Male _ _)) = True
             isMale _ = False

{-}
insertd :: Dynasty -> [Dynasty] -> [Dynasty]
insertd dy dys = insert dy into sorted position in dys (see sortds above)
-}

aliveafter :: String -> [Person] -> [String]
aliveafter name ps = alivein afters
    where getName (Person _ _ pname) = pname
          a:afters = dropWhile (eqName) ps
          eqName (Person _ _ pname) = name /= pname

alivein :: [Person] -> [String]
alivein [] = []
alivein ((Person _ Dead name):ps) = alivein ps
alivein ((Person _ _ name):ps) = name:alivein ps



exdyn = Descend (Person Male Dead "George5") [
  Descend (Person Male Abdicated "Edward8") [],
  Descend (Person Male Dead "George6") [
    Descend (Person Female Alive "Elizabeth2") [
      Descend (Person Male Alive "Charles") [
        Descend (Person Male Alive "William") [
          Descend (Person Male Alive "George") []
        ],
        Descend (Person Male Alive "Harry") []
      ],
      Descend (Person Female Alive "Anne") [
        Descend (Person Male Alive "Peter") [
          Dull (Person Female Alive "Savannah"),
          Dull (Person Female Alive "Isla")
        ],
        Dull (Person Female Alive "Zarah")
      ],
      Descend (Person Male Alive "Andrew") [
        Dull (Person Female Alive "Beatrice"),
        Dull (Person Female Alive "Eugenie")
      ],
      Descend (Person Male Alive "Edward") [
        Dull (Person Female Alive "Louise"),
        Dull (Person Male Alive "James")
      ]
    ],
    Descend (Person Female Dead "Margaret") [
      Dull (Person Male Alive "David"),
      Dull (Person Female Alive "Sarah")
    ]
  ],
  Dull (Person Female Dead "Mary"),
  Dull (Person Male Dead "Henry"),
  Dull (Person Male Dead "George"),
  Dull (Person Male Dead "John")]

