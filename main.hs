-- Food --

data Food = Apple | Mango

render :: Food -> String
render Apple = "🍎"
render Mango = "🥭"

-- Show instances --

instance Show Food where
    show a = render a

-- IO --

main :: IO ()
main =  do
    print(Apple)
    print(Mango)