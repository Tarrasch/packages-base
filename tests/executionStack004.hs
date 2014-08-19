import GHC.ExecutionStack
import Data.List (isInfixOf)

-- In this test we check unloading and loading of dwarf

main :: IO ()
main = do
    someStack <- currentExecutionStack
    testThatItStartsUnloaded someStack
    testThatInDwarfWorksAndCanUnloadAfterwards someStack

testThatItStartsUnloaded stack = do
    frames <- getStackFramesNoSync stack
    check $ "Data not found" `isInfixOf` show frames

testThatInDwarfWorksAndCanUnloadAfterwards stack = do
    frames <- getStackFrames stack
    check $ not $ "Data not found" `isInfixOf` show frames
    dwarfTryUnload >>= check
    dwarfTryUnload >>= (check . not)

check :: Bool -> IO ()
check True  = putStrLn "OK"
check False = putStrLn "Not OK"
