import Machine.Test.Add
import Machine.Test.GCD

main :: IO ()
main = do
    putStrLn "\n============ Add ============"
    addWorks
    addOverflow

    putStrLn "\n============ GCD ============"
    gcdWorks