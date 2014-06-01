{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Main where

import GHC.ExecutionStack
import GHC.IO (IO (..))
import Data.List (isPrefixOf, isSuffixOf)

main = peculiarFunctionName 5 >>= print

checkSensibility :: [StackFrame] -> String
checkSensibility units = str1 ++ str2
    where
        infos = concatMap locationInfos units
        foundPeculiar = any isPeculiar infos
        str1 | foundPeculiar = "Found the peculiar function.\n"
             | otherwise     = "Didn't find the function. :(\n"
        str2 = ""


isPeculiar :: LocationInfo -> Bool
isPeculiar locationInfo = True
        && slin startLine
        && scol startCol
        && elin endLine
        && ecol endCol
        && ("peculiarFunctionName" `isPrefixOf` functionName locationInfo)
        && ("executionStack001.hs" `isSuffixOf` fileName     locationInfo)
    where
        slin = within 100 200
        scol = within   1 100
        elin = within 100 200
        ecol = within   1 100
        within lb ub destructor = let v = destructor locationInfo
                                  in lb <= v && v <= ub

-- (scroll down)
























-- (further down)











































-- XXX: The Line numbers, column numbers and function names matter here!
-- If you move this code, make the appropriate changes in isPeculiar
peculiarFunctionName :: Int -> IO Int
peculiarFunctionName 0 = do
        -- Ok, by now, since we've reached the basecase, we should actually
        -- have some interesting stuff on the executionStack
        stack <- currentExecutionStack
        stackFrames <- getStackFrames stack
        putStrLn $ checkSensibility stackFrames
        return 0
peculiarFunctionName x = IO $ \s ->
    let (IO f) = peculiarFunctionName (x-1)
        (# s', res #) = f s
    in  (# s', res + x #)
      -- By doing like this, we force haskell to put there the update frames
