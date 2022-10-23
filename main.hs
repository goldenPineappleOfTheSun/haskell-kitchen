-- Food --

data FoodKind = Apple | Mango | Bread | Chicken | Egg | Omelete | Sandwich

data State = Normal | Bad | Fried | Boiled | Steamed

-- kind, state, icon
data Food = NoFood | Food { kind :: FoodKind, state :: State, icon :: String}
createFood :: FoodKind -> Food
createFood Apple state = Food {kind=Apple, state=state, icon="🍎"}
createFood Mango state = Food {kind=Mango, state=state, icon="🥭"}
createFood Bread state = Food {kind=Bread, state=state, icon="🍞"}
createFood Chicken state = Food {kind=Chicken, state=state, icon="🍗"}
createFood Egg state = Food {kind=Egg, state=state, icon="🥚"}
createFood Omelete state = Food {kind=Omelete, state=state, icon="🍳"}
createFood Sandwich state = Food {kind=Sandwich, state=state, icon="🥪"}

renderFood :: Food -> String
renderFood (Food kind state icon) = icon ++ ":" ++ show kind ++ ":" ++ show state
renderFood NoFood = "🚫"

matchFood :: Food -> Food -> Bool
matchFood (Food kind_a _ _) (Food kind_b _ _) = kind_a == kind_b
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

_removeFromStorage :: [Food] -> Food -> [Food]
_removeFromStorage [] f = []
_removeFromStorage (x:xs) f = if x == f then xs else _removeFromStorage xs f

removeFromStorage :: Storage -> Food -> Storage
removeFromStorage fridge@(Fridge{slots=s}) f = fridge{slots=_removeFromStorage s f}
removeFromStorage freezer@(Freezer{slots=s}) f = freezer{slots=[]}
removeFromStorage shelve@(Shelve{slots=s}) f = shelve{slots=_removeFromStorage s f}

_findInStorage :: [Food] -> Food -> Food
_findInStorage [] food = NoFood
_findInStorage (x:xs) food = if (matchFood x food) then x else (_findInStorage xs food)

findInStorage :: Storage -> Food -> (Storage, Food)
findInStorage fridge@(Fridge{slots=s, freezer=fz@(Freezer{slots=fs})}) f = 
    if (snd intermediateResult) == NoFood then intermediateResult else (fridge{freezer=removeFromStorage fz f}, _findInStorage fs f)
    where
        intermediateResult = (removeFromStorage fridge f, _findInStorage s f)
        
findInStorage freezer@(Freezer{slots=s}) f = (removeFromStorage freezer f, _findInStorage s f)
findInStorage shelve@(Shelve{slots=s}) f = (removeFromStorage shelve f, _findInStorage s f)

renderStorage :: Storage -> String
renderStorage (Fridge c s freezer) = "❄️" ++ show s ++ "+" ++ show freezer
renderStorage (Freezer c s) = "☃️️" ++ show s
renderStorage (Shelve c s) = "🧳" ++ show s

-- Stove --

data Stove = Stove

fryOnStove :: Stove -> Food -> Food
fryOnStove _ (Food Chicken state icon) = (Food Chicken Fried icon)
fryOnStove _ (Food Bread state icon) = (Food Bread Fried icon)
fryOnStove _ (Food kind state icon) = (Food kind Bad icon)

boilOnStove :: Stove -> Food -> Food
boilOnStove _ (Food Chicken state icon) = (Food Chicken Boiled icon)
boilOnStove _ (Food kind state icon) = (Food kind Bad icon)

steamOnStove :: Stove -> Food -> Food
steamOnStove _ (Food Chicken state icon) = (Food Chicken Steamed icon)
steamOnStove _ (Food kind state icon) = (Food kind Bad icon)

-- Show instances --

instance Show Food where
    show a = renderFood a
    
instance Show Storage where
    show a = renderStorage a
    
instance Show FoodKind where
    show Apple = "яблоко"
    show Mango = "манго"
    show Bread = "хлеб"
    show Chicken = "кура"
    show Egg = "яйцо"
    show Omelete = "яишница"
    show Sandwich = "бутерброд"
    
instance Show State where
    show Normal = "хороший"
    show Bad = "плохой"
    show Fried = "жаренный"
    show Boiled = "варёный"
    show Steamed = "тушёный"
    
-- Eq instances --

instance Eq FoodKind where
    Apple == Apple = True
    Mango == Mango = True
    Bread == Bread = True
    Chicken == Chicken = True
    Egg == Egg = True
    Omelete == Omelete = True
    Sandwich == Sandwich = True
    _ == _ = False

instance Eq State where
    Normal == Normal = True
    Bad == Bad = True
    Fried == Fried = True
    _ == _ = False

instance Eq Food where
    (Food kind_a state_a _) == (Food kind_b state_b _) = kind_a == kind_b && state_a == state_b
    _ == _ = False

-- Receipts --

data Receipt = Receipt String [Food] FoodKind

findFoodInReceipt :: Food -> [Food] -> Bool
findFoodInReceipt food [] = False
findFoodInReceipt food (x:xs) = if x == food then True else findFoodInReceipt food xs  

checkReceipt :: [Food] -> Receipt -> Bool
checkReceipt [] receipt = False
checkReceipt (x:xs) receipt@(Receipt name arr result) = if findFoodInReceipt x arr then True else checkReceipt xs receipt

cookReceipt :: Receipt -> Food
cookReceipt (Receipt name foods result) = (createFood result)

mix :: [Food] -> [Receipt] -> Food
mix [] _ = NoFood
mix foods (x:xs) = if checkReceipt foods x then cookReceipt x else mix foods xs

-- IO --

main :: IO ()
main =  do
    print(findInStorage (putInFreezer (createFridge 20 10) [chick]) chick)
    print(chick)
    print(fridge)
    print(cooked_chick)
    print(fridge_upd_2)
    print(cooked_egg)
    print(bread)
    print(sandwich)
    where
        book_of_receipts = [Receipt "бутерброд" [(createFood Bread), (createFood Chicken)] Sandwich]
        chick = createFood Chicken
        fridge = putInFreezer (createFridge 20 10) [chick]
        (fridge_upd_1, got_chick) = findInStorage fridge chick
        stove = Stove
        cooked_chick = fryOnStove stove chick
        fridge_upd_2 = putInStorage fridge_upd_1 [cooked_chick]
        egg = createFood Egg
        cooked_egg = fryOnStove stove egg
        bread = createFood Bread
        sandwich = mix [bread, cooked_chick] book_of_receipts