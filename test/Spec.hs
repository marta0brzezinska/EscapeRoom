import ParserTest as PT
import ItemTest as IT
import RoomTest as RT
import ActionTest as AT
import GameTest as GT

main :: IO ()
main = do
    putStrLn "Testy modułu Parser:"
    PT.test
    putStrLn "Testy modułu Item:"
    IT.test
    putStrLn "Testy modułu Room:"
    RT.test
    putStrLn "Testy modułu Action:"
    AT.test
    putStrLn "Testy modułu Game:"
    GT.test
