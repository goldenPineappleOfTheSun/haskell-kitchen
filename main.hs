-- Food --

data State = Normal | Bad | Fried

data Food = NoFood | Apple State | Mango State

renderFood :: Food -> String
renderFood (Apple s) = "🍎" ++ ":" ++ "яблоко" ++ ":" ++ show s
renderFood (Mango s) = "🥭" ++ ":" ++ "манго" ++ ":" ++ show s
renderFood NoFood = "🚫"

matchFood :: Food -> Food -> Bool
matchFood (Apple _) (Apple _) = True
matchFood (Apple _) _ = False
matchFood _ _ = False

-- Storages --

data Storage = Fridge {capacity :: Int, slots :: [Food], freezer :: Storage} | Freezer {capacity :: Int, slots :: [Food]} | Shelve {capacity :: Int, slots :: [Food]}

createFridge :: Int -> Int -> Storage
createFridge x y = Fridge {capacity = x, slots = [], freezer = createFreezer y}

createFreezer :: Int -> Storage
createFreezer x = Freezer {capacity = x, slots = []}

createShelve :: Int -> Storage
createShelve x = Shelve {capacity = x, slots = []}

putInStorage :: Storage -> [Food] -> Storage
putInStorage fridge@(Fridge{capacity=c, slots=s, freezer = fz}) [] = fridge
putInStorage fridge@(Fridge{capacity=c, slots=s, freezer = fz}) (x:xs) = if (length s < c) then putInStorage (Fridge {capacity = c, slots = x:s, freezer = fz}) xs else fridge 
putInStorage freezer@(Freezer{capacity=c, slots=s}) [] = freezer
putInStorage freezer@(Freezer{capacity=c, slots=s}) (x:xs) = if (length s < c) then putInStorage (Freezer {capacity = c, slots = x:s}) xs else freezer 
putInStorage shelve@(Shelve{capacity=c, slots=s}) [] = shelve
putInStorage shelve@(Shelve{capacity=c, slots=s}) (x:xs) = if (length s < c) then putInStorage (Shelve {capacity = c, slots = x:s}) xs else shelve

putInFreezer :: Storage -> [Food] -> Storage
putInFreezer fridge@(Fridge{capacity=c, slots=s, freezer = fz}) food = (Fridge {capacity = c, slots = s, freezer = putInStorage fz food})

_findInStorage :: [Food] -> Food -> Food
_findInStorage [] food = NoFood
_findInStorage (x:xs) food = if (matchFood x food) then x else (_findInStorage xs food)

findInStorage :: Storage -> Food -> Food
findInStorage Fridge{slots=s} f = _findInStorage s f
findInStorage Freezer{slots=s} f = _findInStorage s f
findInStorage Shelve{slots=s} f = _findInStorage s f

renderStorage :: Storage -> String
renderStorage (Fridge c s freezer) = "❄️" ++ show s ++ "+" ++ show freezer
renderStorage (Freezer c s) = "☃️️" ++ show s
renderStorage (Shelve c s) = "🧳" ++ show s

-- Show instances --

instance Show Food where
    show a = renderFood a
    
instance Show Storage where
    show a = renderStorage a
    
instance Show State where
    show Normal = "хороший"
    show Bad = "плохой"
    show Fried = "жаренный"

-- IO --

main :: IO ()
main =  do
    print(got_apple)
    print(apple_1)
    print(apple_2)
    print(mango_1)
    print(fridge)
        where
        apple_1 = Apple Normal
        apple_2 = Apple Normal
        mango_1 = Mango Normal
        fridge = putInFreezer (putInStorage (createFridge 20 10) [apple_1, apple_2, mango_1]) [mango_1, mango_1]
        got_apple = findInStorage fridge apple_1