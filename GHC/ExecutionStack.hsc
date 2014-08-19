-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ExecutionStack
-- Copyright   :  (c) The University of Glasgow 2013-2014
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This is a module for the efficient but inaccurate stack traces. If you
-- can take a factor 2 of performance penalty, you should consider using
-- "GHC.Stack" as the stack traces will be more accurate and detailed.
--
-- @
-- myFunction :: IO ()
-- myFunction = do
--      stack <- currentExecutionStack
--      printExecutionStack stack
-- @
--
-- An 'ExecutionStack' is a data wrapper around 'ByteArray#'. The
-- Array is a reified stack. Each element is a code address. For most
-- frames this will be the return address for the stack frame, but
-- for an update frame the address is the entry code of the thunk (if
-- available)
--
-- /Since: 4.7.0.0/
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedTuples, MagicHash, RecordWildCards #-}
module GHC.ExecutionStack (
  -- * Simple interface
    currentExecutionStack
  , printExecutionStack
  -- * Intermediate interface
  , ExecutionStack ()
  , getStackFrames
  , StackFrame(..)
  , LocationInfo(..)
  , printExecutionStackRts
  -- * Complicated interface
  -- ** Managed loading/unloading
  , dwarfIncRef
  , dwarfDecRef
  , dwarfTryUnload
  -- ** Forceful loading/unloading
  , dwarfForceLoad
  , dwarfForceUnload
  -- ** Destructing `ExecutionStack`
  , stackSize
  , stackIndex
  , stackIndices
  -- ** Looking up instructions
  , getStackFramesNoSync
  , getStackFrame
  , getStackFrameCustom
  , getStackFrameNoSync
  , getStackFrameCustomNoSync
  ) where

import GHC.IO (IO(..))
import GHC.Exts
import GHC.Word (Word16)
import Foreign.C.String (peekCString, CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (alloca, allocaArray)
import Text.Printf (printf)
import Control.Exception.Base (bracket_)

#include "Rts.h"

data ExecutionStack = ExecutionStack
    { unExecutionStack ::  ByteArray##
    }

-- | The number of functions on your stack
stackSize :: ExecutionStack -> Int
stackSize stack =
    I## (sizeofByteArray## (unExecutionStack stack)) `div` (#const SIZEOF_HSPTR)

stackIndex :: ExecutionStack -> Int -> Ptr Instruction
stackIndex (ExecutionStack ba##) (I## i##) = Ptr (indexAddrArray## ba## i##)

stackIndices :: ExecutionStack -> [Ptr Instruction]
stackIndices stack = map (stackIndex stack) [0..(stackSize stack)-1]

data StackFrame = StackFrame
    { unitName      :: !String -- ^ From symbol table
    , procedureName :: !String -- ^ Also from symbol table
    , locationInfos :: ![LocationInfo] -- ^ Empty without @-g@ flag to @ghc@
    }
  -- Looking at Dwarf.h, this is one DwarfUnit and many DebugInfos.

instance Show StackFrame where
    show = showStackFrame

-- | Location in source code.
--
-- TODO(arash, peter): We should aim to make all the Word16s into proper Ints.
data LocationInfo = LocationInfo
    { startLine    :: !Word16
    , startCol     :: !Word16
    , endLine      :: !Word16
    , endCol       :: !Word16
    , fileName     :: !String
    , functionName :: !String
    }
    deriving(Eq)
  -- This struct matches the C struct @DebugInfo_@, from Dwarf.h

instance Show LocationInfo where
    show = showLocationInfo

-- TODO: Better name than prepareStackFrame?

-- | Like 'show', without @unlines@
prepareStackFrame :: StackFrame -> [String]
prepareStackFrame su | null (locationInfos su) =
        [procedureName su ++ " (using " ++ unitName su ++ ")"]
    --  example: mySrcFun (using /path/lib.so)
prepareStackFrame su | otherwise = map showLocationInfo $ locationInfos su

showStackFrame :: StackFrame -> String
showStackFrame = unlines . prepareStackFrame

-- | Pretty print the execution stack to stdout
printExecutionStack :: ExecutionStack -> IO ()
printExecutionStack stack = do
    frames <- getStackFrames stack
    putStrLn $ displayFramesWithHeader frames

displayFramesWithHeader :: [StackFrame] -> String
displayFramesWithHeader frames =
    "Stack trace (Haskell):\n" ++
    concatMap (uncurry displayFrame) ([0..] `zip` frames)

-- | How one StackFrame is displayed in one stack trace
displayFrame :: Int -> StackFrame -> String
displayFrame ix frame = unlines $ zipWith ($) formatters strings
      where formatters = (printf "%4u: %s" (ix :: Int)) : repeat ("      " ++)
            strings    = prepareStackFrame frame

showLocationInfo :: LocationInfo -> String
showLocationInfo LocationInfo{..} =
    functionName ++
    " (at " ++ fileName ++
    ":" ++ show startLine ++ ":" ++ show startCol ++
    "-" ++ show endLine ++ ":" ++ show endCol ++
    ")"
    -- example: "bindIO (at libraries/base/GHC/Base.lhs:609:61-609:77)"

instance Storable LocationInfo where
    sizeOf _ = (#size struct DebugInfo_)
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
      startLine    <- #{peek struct DebugInfo_, sline } ptr
      startCol     <- #{peek struct DebugInfo_, scol  } ptr
      endLine      <- #{peek struct DebugInfo_, eline } ptr
      endCol       <- #{peek struct DebugInfo_, ecol  } ptr
      fileName     <- #{peek struct DebugInfo_, file  } ptr >>= peekCString
      functionName <- #{peek struct DebugInfo_, name  } ptr >>= peekCString
      return LocationInfo {..}

    poke ptr (LocationInfo{..}) =
        error "Sorry, we're not really Storable, just use it for peek :("

-- We use these three empty data declarations for type-safety
data DwarfUnit
data DwarfProc
data Instruction

peekDwarfUnitName :: Ptr DwarfUnit -> IO CString
peekDwarfUnitName ptr = #{peek struct DwarfUnit_, name } ptr

peekDwarfProcName :: Ptr DwarfProc -> IO CString
peekDwarfProcName ptr = #{peek struct DwarfProc_, name } ptr

-- | Reify the stack. This is the only way to get an ExecutionStack value.
currentExecutionStack :: IO (ExecutionStack)
currentExecutionStack =
    IO (\s -> let (## new_s, byteArray## ##) = reifyStack## s
                  ba = ExecutionStack byteArray##
              in (## new_s, ba ##) )

-- | Pretty print the stack. Will print it to stdout. Note that this
-- function is faster since it is implemented directly in the RTS.
printExecutionStackRts :: ExecutionStack -> IO ()
printExecutionStackRts (ExecutionStack ba) =
    IO (\s -> let new_s = dumpStack## ba s
              in (## new_s, () ##))

-- | Tell the dwarf module that you want to use dwarf. Synchronized.
foreign import ccall "Dwarf.h dwarf_inc_ref" dwarfIncRef :: IO ()

-- | Tell the dwarf module that you are done using dwarf. Synchronized.
foreign import ccall "Dwarf.h dwarf_dec_ref" dwarfDecRef :: IO ()

-- | Ask the dwarf module if it can unload and free up memory. It will not be
-- able to if the module is in use or if data is not loaded. Synchronized.
--
-- Returns True if dwarf data was unloaded.
foreign import ccall "Dwarf.h dwarf_try_unload" dwarfTryUnload :: IO Bool

foreign import ccall "Dwarf.h dwarf_force_load" dwarfForceLoad :: IO ()

foreign import ccall "Dwarf.h dwarf_force_unload" dwarfForceUnload :: IO ()

-- | For the given instruction pointer, how many LocationInfos does it have?
--
-- Dwarf module must be loaded to use this!
foreign import ccall "Dwarf.h dwarf_addr_num_infos"
    dwarfAddrNumInfos :: Ptr Instruction -> IO CInt

-- | Ask dwarf module for
--
-- Dwarf module must be loaded to use this!
foreign import ccall "Dwarf.h dwarf_lookup_ip"
    dwarfLookupIp ::
       Ptr Instruction -- ^ Code address you want information about
    -> Ptr (Ptr DwarfProc) -- ^ Out: DwarfProc Pointer Pointer
    -> Ptr (Ptr DwarfUnit) -- ^ Out: DwarfUnit Pointer Pointer
    -> Ptr LocationInfo -- ^ Out: LocationInfos to write (array)
    -> CInt -- ^ Max amount of LocationInfo it should write (capacity of array)
    -> IO CInt -- ^ How many LocationInfos was actually written

inDwarf :: IO a -> IO a
inDwarf = bracket_ dwarfIncRef dwarfDecRef

getStackFrameCustom ::
       Ptr Instruction -- ^ Instruction Pointer
    -> Int -- ^ Max amount to write
    -> IO StackFrame -- ^ Result
getStackFrameCustom ip maxNumInfos =
    inDwarf $ getStackFrameCustomNoSync ip maxNumInfos

getStackFrameCustomNoSync ::
       Ptr Instruction -- ^ Instruction Pointer
    -> Int -- ^ Max amount to write
    -> IO StackFrame -- ^ Result
getStackFrameCustomNoSync ip maxNumInfos = do
    alloca $ \ppDwarfProc -> do
      poke ppDwarfProc nullPtr
      alloca $ \ppDwarfUnit ->
        allocaArray maxNumInfos $ \infos -> do
          numWritten <- dwarfLookupIp ip
                                      ppDwarfProc
                                      ppDwarfUnit
                                      infos
                                      cMaxNumInfos
          pDwarfProc <- peek ppDwarfProc
          pDwarfUnit <- peek ppDwarfUnit
          unitName <- stringPeekWith peekDwarfUnitName pDwarfUnit
          procedureName <- stringPeekWith peekDwarfProcName pDwarfProc
          locationInfos <- mapM (peekElemOff infos)
                                [0..(fromIntegral numWritten)-1 :: Int]
          return StackFrame{..}
  where
    cMaxNumInfos = fromIntegral maxNumInfos

-- Note: if you grepped your way to the string "<Data not found>,
-- you probably forgot to compile that module with the `-g` flag to ghc.
stringPeekWith :: (Ptr a -> IO CString) -> Ptr a -> IO String
stringPeekWith peeker ptr | ptr == nullPtr = return "<Data not found>"
stringPeekWith peeker ptr | otherwise      = peeker ptr >>= peekCString

getStackFrame :: Ptr Instruction -> IO StackFrame
getStackFrame ip = inDwarf $ getStackFrameNoSync ip

getStackFrameNoSync :: Ptr Instruction -> IO StackFrame
getStackFrameNoSync ip =
    dwarfAddrNumInfos ip >>= (getStackFrameCustomNoSync ip . fromIntegral)

getStackFrames :: ExecutionStack -> IO [StackFrame]
getStackFrames stack = inDwarf $ getStackFramesNoSync stack

getStackFramesNoSync :: ExecutionStack -> IO [StackFrame]
getStackFramesNoSync = mapM getStackFrameNoSync . stackIndices
