import GHC.ExecutionStack
import Debug.Trace
import GHC.IO (unsafePerformIO)
import Data.List (isPrefixOf, replicate)

-- Are ticks across specializations maintained?
--
-- I also did eyeball the output of
--
-- $ ./inplace/bin/ghc-stage2 -ddump-simpl -fspecialise -O \
--     libraries/base/tests/executionStack003.hs | less
--
-- There I make sure that the general version gets replaced with one
-- specialized for Int.
--
-- Also, it might be interesting to pass the flags '-g -dppr-ticks' if you
-- want to do manual inspection

myTypeclassFunction :: (Num a) => [a] -> a
{-# SPECIALISE myTypeclassFunction :: [Int] -> Int #-}
myTypeclassFunction []     = fromIntegral $ unsafePerformIO $ findOccurencesOf "myTypeclassFunction"
myTypeclassFunction (x:xs) = myTypeclassFunction xs + x

findOccurencesOf :: String -> IO Int
findOccurencesOf fname = do
    stack <- currentExecutionStack
    stackFrames <- getStackFrames stack
    let infos = concatMap locationInfos stackFrames
        functionNames = map functionName infos
    return $ length . filter (isPrefixOf fname) $ functionNames

test1 = putStrLn
      . formatter
      . (myTypeclassFunction :: [Int] -> Int)
      $ replicate 5 (0 :: Int)
     -- XXX: We only recurse a few times in myTypeclassFunction,
     -- otherwise we'll get marked update frames, and we won't be able to
     -- resolve the function names on them.

formatter x | x > 10    = "Wut?"
formatter x | x > 2     = "Dwarf data remains when specializing! :)"
formatter x | otherwise = "Debug data gets lost when specializing"

test2 = putStrLn
      . (\1 -> "Is also kept for 'pre-specialized' function sum")
      . (sum :: [Int] -> Int)
      $ replicate 5 (0 :: Int) ++ [unsafePerformIO $ findOccurencesOf "sum"]

main = test1 >> test2
