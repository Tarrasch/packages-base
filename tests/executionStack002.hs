import GHC.ExecutionStack
import GHC.IO (unsafePerformIO)

-- In this test we check that the stack reification can walk along big
-- stacks too

main = print $ recurseAlot 10000

recurseAlot 0 = unsafePerformIO $ do
        stack <- currentExecutionStack
        putStrLn $ formatter (stackSize stack)
        return 0
recurseAlot x = recurseAlot (x-1) + 1

formatter x | x < 5000  = "Trace to small"
formatter x | x > 50000 = "Trace to big"
formatter x | otherwise = "Nice stack! :)"

-- XXX: This test might fail in the future if GHC gets much smarter and
-- realizes that it doesn't need to create an update frame for each (+ 1),
-- rather it can add 10000 right away.
