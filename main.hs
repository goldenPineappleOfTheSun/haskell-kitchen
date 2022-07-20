-- Food --

data Food = Apple | Mango

renderFood :: Food -> String
renderFood Apple = "🍎"
renderFood Mango = "🥭"

-- Storages --

data Storage = Fridge {capacity :: Integer, slots :: [Food]}

createFridge :: Integer -> Storage
createFridge x = Fridge {capacity = x, slots = []}

putInStorage :: Storage -> [Food] -> Storage
putInStorage Fridge{capacity=c, slots=s} [] = Fridge {capacity = c, slots = s}
putInStorage Fridge{capacity=c, slots=s} (x:xs) = putInStorage (Fridge {capacity = c, slots = x:s}) xs

renderStorage :: Storage -> String
renderStorage (Fridge c s) = "❄️" ++ show s

-- Show instances --

instance Show Food where
    show a = renderFood a
    
instance Show Storage where
    show a = renderStorage a

-- IO --

main :: IO ()
main =  do
    print(apple_1)
    print(apple_2)
    print(mango_1)
    print(fridge)
    where
    apple_1 = Apple
    apple_2 = Apple
    mango_1 = Mango
    fridge = putInStorage (createFridge 20) [apple_1, apple_2, mango_1]