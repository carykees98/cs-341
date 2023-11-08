module Hw4 (Family (..), addPerson, addPeople, somebody, rmPerson, birthday, justJust, voting, getAge) where

{-
Family is a recursive data structure representing a family.
Each person in the family has a name, age, and a list of their children.
If they have no children, then that list will be empty.

For the purposes of this problem, everybody has only one parent,
  except for one person at the root of the family with no parent.

A Family is also allowed to have the value Nobody.  That value
will only be used when there is nobody in the family.

Here is an example of a Family:
Person "a" 20 [Person "d" 16 [],Person "b" 14 [Person "e" 10 [Person "f" 12 []],Person "c" 19 []],Person "g" 10 []]
In this family, a is the parent of d, b and g.  b is the parent of c and e.  e is the parent of f.
Also, a is 20 years old, etc.
Don't worry that the ages don't make logical sense in my example.
-}
type Name = String

type Age = Int

data Family = Nobody | Person Name Age [Family]
  deriving (Eq)

--  deriving (Eq, Show)

{-
Make Family an instance of Show
If Nobody is in the familly, it returns "There is nobody in the family"
If a Person has no children, it returns the name and age, separated by a colon
If a Person has children, it return the name and age (seaparated by colon), followed by a list of children
To use this: don't forget to delete undo "deriving Show" from above
The example from the first problem is shown as:
a:20[d:16,b:14[e:10[f:12],c:19],g:10]
-}
instance Show Family where
  -- Your code goes here and uncomment previous line
  show Nobody = "There is nobody in the family"
  show (Person name age []) = name ++ ":" ++ show age
  show (Person name age family) = name ++ ":" ++ show age ++ show family

{-
addPerson adds a Person to the Family.
It takes a triple conatining: a person's name, their age and their parent
  and a Family
  and adds that Person to the Family in the proper place.

If the parent is not in the Family, the function does not add the Person

One exception is that if nobody is the Family, then that Person will become the only one in the Family
  and the given parent will be ignored.

Here are some examples.
See definitions of e1, e2, e3, e4 below:
\*Hw4> e1
ann:1
\*Hw4> e2
ann:1[bill:2]
\*Hw4> e3
ann:1[chuck:3,bill:2]
\*Hw4> e4
ann:1[chuck:3,bill:2[dave:4]]
\*Hw4> addPerson ("ed",20,"fred") e4
ann:1[chuck:3,bill:2[dave:4]]
-}
addPerson :: (Name, Age, Name) -> Family -> Family
addPerson (newName, newAge, _) Nobody = Person newName newAge []
addPerson (newName, newAge, parent) (Person name age [])
  | parent == name = Person name age [Person newName newAge []]
  | otherwise = Person name age []
addPerson newPerson@(newName, newAge, parent) (Person name age family)
  | parent == name = Person name age (Person newName newAge [] : family)
  | otherwise = Person name age $ map (addPerson newPerson) family

e1 :: Family
e1 = addPerson ("ann", 1, "blah") Nobody

e2 :: Family
e2 = addPerson ("bill", 2, "ann") e1

e3 :: Family
e3 = addPerson ("chuck", 3, "ann") e2

e4 :: Family
e4 = addPerson ("dave", 4, "bill") e3

{-
addPeople takes a list of triples, as in the previous problem
and creates a Family with all those people, adding them in order from left to right
Example:
\*Hw4> addPeople [("ann",1,"blah"),("bill",2,"ann"),("chuck",3,"ann"),("dave",4,"bill"),("ed",20,"fred")]
ann:1[chuck:3,bill:2[dave:4]]
-}
addPeople :: [(Name, Age, Name)] -> Family
-- Your code goes here
addPeople [] = Nobody
addPeople people = addPerson (last people) (addPeople (init people))

{-
somebody takess a list of families and returns a list of all families in that list that are not Nobody
Example:
\*Hw4> somebody [e1,Nobody,e2,Nobody,e3]
[ann:1,ann:1[bill:2],ann:1[chuck:3,bill:2]]
-}
somebody :: [Family] -> [Family]
-- Your code goes here
somebody families = filter (\x -> x /= Nobody) families

{-
rmPerson takes a Name and a Family
It removes the Person with that name and all their descendants from the Family
Example:
\*Hw4> rmPerson "bill" e4
ann:1[chuck:3]
\*Hw4> rmPerson "ann" e4
There is nobody in this family
-}
rmPerson :: Name -> Family -> Family
-- Your code goes here
rmPerson name (Person name2 age family)
  | name == name2 = Nobody
  | otherwise = Person name2 age (somebody (map (rmPerson name) family))

{-
birthday takes a Name and a Family
It adds 1 to the Age of the Person with that Name
If the Person is not in the Family it does nothing

Example:
\*Hw4> birthday "bill" e4
ann:1[chuck:3,bill:3[dave:4]]
-}
birthday :: Name -> Family -> Family
-- Your code goes here
birthday name (Person name2 age family)
  | name == name2 = Person name2 (age + 1) family
  | otherwise = Person name2 age (map (birthday name) family)

{-
voting takes a family as argument
it returns a list of the names of everybody in that Family who is at least 18 years old

Example:
\*Hw4> voting e4
[]
-}
voting :: Family -> [Name]
-- Your code goes here
voting (Person name age family)
  | age >= 18 = name : concatMap voting family
  | otherwise = concatMap voting family

{-
justJust takes a list of Maybe a's
if everything in the list is Nothing it returns Nothing
  otherwise it returns the first element in the list that is not nothing
\*Hw4> justJust [Nothing,Nothing,Just 2,Nothing,Just 4,Nothing]
Just 2
-}
justJust :: Eq a => [Maybe a] -> Maybe a
-- Your code goes here
justJust [] = Nothing
justJust (Nothing : maybes) = justJust maybes
justJust (Just x : maybes) = Just x

{-
getAge takes a Name and a Family
If the Person with that Name is in the Family, it returns Just their age
Otherwise it returns Nothing

Example:
\*Hw4> getAge "bill" e4
Just 2
\*Hw4> getAge "fred" e4
Nothing
-}
getAge :: Name -> Family -> Maybe Age
-- Your code goes here
getAge _ Nobody = Nothing
getAge name (Person name2 age family)
  | name == name2 = Just age
  | otherwise = justJust (map (getAge name) family)

e :: Family
e = addPeople [("a", 20, ""), ("g", 10, "a"), ("b", 14, "a"), ("c", 19, "b"), ("d", 16, "a"), ("e", 10, "b"), ("f", 12, "e")]
