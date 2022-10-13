--We can use the let keyword to define a name right in GHCI. Doing let a = 1 inside GHCI is the equivalent of writing a = 1 in a script and then loading it.
--ghci> let lostNumbers = [4,8,15,16,23]
lostNumbers = [4,8,15,16,23]
firstList = [1,2,3]
secondList = [7,8,9]
x :: Int
x = 1

--(append)
--Haskell has to walk through the whole list on the left side of ++ and then append the second list
--[<list>] ++ [<list>]
--[1,2,3] ++ [7,8,9] -> [1,2,3,7,8,9]
--"hello" ++ " " ++ "world" -> "hello world"
--['w','h'] ++ ['a','t'] -> "what"
appendedList = firstList ++ secondList

--cons operator (prepend) puts something at the beginning of the list
-- <number/char/list> : [<list>]
--5 : [1,2] -> [5,1,2]
--5 : 3 : [] -> [5,3]
prependedList = 5 : appendedList

--[] empty list
--[[]] list which contains an empty list

--getting an element from a list is by !!<pos> (elements start from zero)
--lostNumbers !!3 -> 16
--"marto" !!0 -> 'm'
thirdNumInList = prependedList !! 2

--lists in lists
--The lists within a list can be of different lengths but they can't be of different types
listInList = [[1,2,3],[4,5,6],[7,8,9]]
appendList = listInList ++ [[0]]
prependList = [0] : listInList
getList = listInList !! 1

--comparison
--in lexicographical order. First the heads are compared. If they are equal then the second elements are compared, etc.
areNotEqual = firstList == secondList
areEqual = firstList == firstList
greater = firstList > secondList
less = firstList < secondList

--functions:
--When using head, tail, last and init, be careful not to use them on empty lists.
--head [<list>] takes the first element from the list 
--tail [<list>] takes everything exept for the head(first one)
--init [<list>] takes everything except for the last element
--last [<list>] takes the last element of the list
--length [<list>] tells the length of the list

--null [] -> true , null [1,2,3] -> false (same as xs == [], where xs is a list)
notNullList = [1,2,3]
nullList = []

--reverse [<list>] reverses the order of the elemnts in the list
reverseList = reverse lostNumbers

--take <num of elem from the beginning> [<list>] take the n first elements from a list (take 5 [1,2] -> [1,2])
extractedList = take 2 lostNumbers
extractedNullList = take 0 lostNumbers

--drop <num of elem from the beginning> [<list>] drop the n fisrt elements from a list (a.k.a skip them and save the remainder)
droppedList = drop 2 lostNumbers
droppedNotNullList = drop 0 lostNumbers
droppedNullList = drop 100 lostNumbers

--maximum
--minimum
maxElem = maximum lostNumbers
minElem = minimum lostNumbers

--elem takes a thing and a list of things and tells us if that thing is an element of the list
isExisting = 23 `elem` lostNumbers