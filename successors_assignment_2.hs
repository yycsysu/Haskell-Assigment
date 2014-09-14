import Data.List

data Sex = Male | Female deriving (Eq, Show)
instance Ord Sex where
    compare Male Male = EQ
    compare Male Female = LT
    compare Female Male = GT
    compare Female Female = EQ

data Status = Alive | Dead | Abdicated deriving (Eq, Show)
data Person = Person Sex Status String deriving (Eq, Show)
instance Ord Person where
    compare (Person sx1 _ _) (Person sx2 _ _) = compare sx1 sx2

data Dynasty = Descend Person [Dynasty] | Dull Person deriving (Eq, Show)
instance Ord Dynasty where
    compare (Descend p1 _) (Descend p2 _) = compare p1 p2
    compare (Descend p1 _) (Dull p2 ) = compare p1 p2
    compare (Dull p1 ) (Descend p2 _) = compare p1 p2
    compare (Dull p1) (Dull p2 ) = compare p1 p2

successors :: String -> Dynasty -> [String]
successors name dynasty = aliveafter name (linefrom dynasty)

-- define the catamorphism cataD on Dynasty
-- then reimplement linefrom to use cataD instead of explicit
-- recursion and the new version of reorder below

cataD :: (Dynasty -> result, Dynasty -> [result] -> result) -> Dynasty -> result
cataD agebra@(f_d, _) descend@(Descend person []) = f_d descend
cataD agebra@(f_d, _) dull@(Dull person) = f_d dull
cataD agebra@(f_d, f_dys) descend@(Descend person dys) = f_dys descend (map (cataD agebra) dys)

-- cataD agebra@(f_p, f_descend) (Descend person dys) = sortds (cataD agebra dys)
-- cataD agebra@(f_p, _) dull@(Dull person) = dull

linefrom :: Dynasty -> [Person]
linefrom dy = cataD (f_d, f_dys) dy
    where f_d (Descend (Person _ Abdicated _) _) = []
          f_d (Dull (Person _ Abdicated _)) = []
          f_d (Descend person _) = [person]
          f_d (Dull person) = [person]
          f_dys (Descend (Person _ Abdicated _) _) pss = []
          f_dys (Descend person _) pss = person:concat (sort pss)
          isMale [] = False
          isMale ((Person sex _ _):ps) = sex == Male
          sort pss = ms ++ fms
              where (ms, fms) = partition (isMale) pss

-- redefine reorder so that all sub-dynasties are sorted
-- with Males before Females, using cataD
reorder :: Dynasty -> Dynasty
reorder dy = cataD (\a -> a, \(Descend person _) dys -> (Descend person (sortds dys))) dy

sortds :: [Dynasty] -> [Dynasty]
-- reimplement sortds to use new insertd and flatten below
sortds dys = flatten $ foldr insertd Dnull dys

-- define a type of binary trees for Dynasty
data BTD = Dnode BTD Dynasty BTD | Dnull
-- define the catamorphism cataBTD on the above type
cataBTD :: (result, result -> Dynasty -> result -> result) -> BTD -> result 
cataBTD agebra@(f_null, f_btd) (Dnode bt1 dy bt2) = f_btd (cataBTD agebra bt1) dy (cataBTD agebra bt2)
cataBTD agebra@(f_null, _) (Dnull) = f_null

-- use cataBTD to define a function to “flatten” btd :: BTD
-- in an in-order traversal
flatten :: BTD -> [Dynasty]
flatten btd = cataBTD (f_null, f_btd) btd
    where f_null = []
          f_btd r1 dy r2 = r1 ++ (dy:r2)
-- redefine insertd so that “flatten d” yields every top-level
-- Dynasty in d headed by a Male before every top-level Dynasty
-- in d headed by a Female, in particular so that
-- “flatten(insertd d btd)” yields d before every top-level Dynasty
-- headed by a Person of the same Sex as d
insertd :: Dynasty -> BTD -> BTD
insertd dy Dnull = Dnode Dnull dy Dnull
insertd dy1 (Dnode bt1 dy2 bt2)
    | dy1 <= dy2 = Dnode (insertd dy1 bt1) dy2 bt2
    | dy1 > dy2 = Dnode bt1 dy2 (insertd dy1 bt2)

aliveafter :: String -> [Person] -> [String]
aliveafter name ps =
    let fromnam = dropWhile (\(Person _ _ pname)-> name /= pname) ps
    in if null fromnam then [] else alivein (tail fromnam)

alivein :: [Person] -> [String]
alivein = map (\(Person _ _ name) -> name) . filter (\(Person _ st _) -> st == Alive)


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
