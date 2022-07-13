-- Food --

data Apple = Apple
data Mango = Mango

-- Storages --

-- Show instances --

instance Show Apple where
    show Apple = "🍎"
    
instance Show Mango where
    show Mango = "🥭"

-- IO --

main :: IO ()
main =  do
    print(Apple)
    print(Mango)