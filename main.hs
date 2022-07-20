-- Food --

data State = Normal | Bad | Fried

data Food = NoFood | Apple State | Mango State | Chicken State

renderFood :: Food -> String
renderFood (Apple s) = "🍎" ++ ":" ++ "яблоко" ++ ":" ++ show s
renderFood (Mango s) = "🥭" ++ ":" ++ "манго" ++ ":" ++ show s
renderFood (Chicken s) = "🍗" ++ ":" ++ "курица" ++ ":" ++ show s
renderFood NoFood = "🚫"

matchFood :: Food -> Food -> Bool
matchFood (Apple _) (Apple _) = True
matchFood (Mango _) (Mango _) = True
matchFood (Chicken _) (Chicken _) = True
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
putInStorage storage [] = storage
putInStorage fridge@(Fridge{capacity=c, slots=s}) (x:xs) = if (length s < c) then putInStorage (fridge {slots = x:s}) xs else fridge 
putInStorage freezer@(Freezer{capacity=c, slots=s}) (x:xs) = if (length s < c) then putInStorage (freezer {slots = x:s}) xs else freezer 
putInStorage shelve@(Shelve{capacity=c, slots=s}) (x:xs) = if (length s < c) then putInStorage (shelve {slots = x:s}) xs else shelve

putInFreezer :: Storage -> [Food] -> Storage
putInFreezer fridge@(Fridge{freezer = fz}) food = (fridge {freezer = putInStorage fz food})

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

-- Stove --

data Stove = Stove

cookOnStove :: Stove -> Food -> Food
cookOnStove _ (Apple s) = Apple Fried
cookOnStove _ (Mango s) = Mango Fried
cookOnStove _ (Chicken s) = Chicken Fried

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
    print(chick)
    print(fridge)
    print(cooked_chick)
    print(updated_fridge)
        where
        chick = Chicken Normal
        fridge = putInFreezer (createFridge 20 10) [chick]
        got_chick = findInStorage fridge chick
        stove = Stove
        cooked_chick = cookOnStove stove chick
        updated_fridge = putInStorage fridge [cooked_chick]