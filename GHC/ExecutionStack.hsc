-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ExecutionStack
-- Copyright   :  (c) The University of Glasgow 2013
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This is a module for the efficient but inaccurate Stack Traces. If you
-- can take a factor 2 of performance penalty. You should consider using
-- "GHC.Stack" as the stack traces will be more accurate and detailed.
--
-- @
-- myFunction :: IO ()
-- myFunction = do
--      stack <- currentExecutionStack
--      dumpExecutionStack stack
-- @
--
-- An 'ExecutionStack' is a data wrapper around 'ByteArray#'. The Array is
-- a reified stack. Each element can be thought as the Instruction
-- Pointers. For languages like C you can see from the instruction pointer
-- where you are in the original Haskell function. The ExecutionStack as
-- described by the STG will only contain pointers to entry code of Info
-- Tables.
--
-- /Since: 4.7.0.0/
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedTuples, MagicHash, RecordWildCards #-}
module GHC.ExecutionStack (
  -- * Simple interface
    currentExecutionStack
  , dumpExecutionStack
  -- * Complicated interface
  -- ** ExecutionStack
  , ExecutionStack ()
  , stackSize
  , stackIndex
  , stackIndexes
  -- ** Structures the stack translates to
  , LocationInfo(..)
  , showLocationInfo
  , StackUnit(..)
  , showStackUnit
  -- * New stuff
  , getStackUnit 
  , getStackUnits
  , getStackUnitCustom
  -- ** Other
  , dwarfInit
  , dwarfFree
  ) where

import GHC.IO (IO(..)
             , unsafePerformIO)
import GHC.Prim
import GHC.Exts
import Unsafe.Coerce
import System.Mem
import Data.Char
import Data.Int
import Data.Word
import GHC.Word
import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Foreign.C.String
import Foreign.C
import Text.Printf (printf)
import Foreign.Storable (Storable(..))
import Foreign.Marshal

#include "Rts.h"

data ExecutionStack = ExecutionStack
    { unExecutionStack ::  ByteArray##
    }

instance Show ExecutionStack where
    show = showExecutionStack
-- | The number of functions on your stack

stackSize :: ExecutionStack -> Int
stackSize stack =
    I## (sizeofByteArray## (unExecutionStack stack)) `div` (#const SIZEOF_HSPTR)

stackIndex :: ExecutionStack -> Int -> Ptr Instruction
stackIndex (ExecutionStack ba##) (I## i##) = Ptr (indexAddrArray## ba## i##)

stackIndexes :: ExecutionStack -> [Ptr Instruction]
stackIndexes stack = map (stackIndex stack) [0..(stackSize stack)-1]


-- TODO: Better name than StackUnit ?
data StackUnit = StackUnit {
    unitName      :: !String
  , procedureName ::  String -- TODO: redo strict
  , locationInfos :: ![LocationInfo]
  }
  -- Looking at Dwarf.h, this is one DwarfUnit and many DebugInfos.

-- TODO: Better name than prepareStackUnit?

-- | Like 'show', but with a row of comments
--
-- Note, only safe when you've not called 'dwarfFree'
prepareStackUnit :: StackUnit -> [String]
prepareStackUnit su | null (locationInfos su) = (:[]) $
        -- unitName su ++
        "???" ++
        " (using " ++ unitName su ++ ")"
    --  mySrcFun (using /path/lib.so)
prepareStackUnit su | otherwise = map showLocationInfo $ locationInfos su

-- | Pretty-print a 'StackUnit'
--
-- Note, only safe when you've not called 'dwarfFree'
showStackUnit :: StackUnit -> String
showStackUnit = unlines . prepareStackUnit

-- | This is a candidate for a Instruction Pointe.  This struct
-- matches the C struct @DebugInfo_@, from dwarf.h
data LocationInfo = LocationInfo {
           startLine    :: !Word16,
           startCol     :: !Word16,
           endLine      :: !Word16,
           endCol       :: !Word16,
           fileName     :: !String,
           functionName :: !String
           }
           deriving(Eq)

-- | Will only work if there have not been any 'dwarfFree' since the value
-- was created. For this reason 'LocationInfo' have no Show instance.
showLocationInfo :: LocationInfo -> String
showLocationInfo LocationInfo{..} =
    functionName ++
    " (at " ++ fileName ++
    ":" ++ show startLine ++ ":" ++ show startCol ++
    "-" ++ show endLine ++ ":" ++ show endCol ++
    ")"
    -- example: "bindIO (at libraries/base/GHC/Base.lhs:609:61-609:77)"
  where

instance Storable LocationInfo where
    sizeOf _ = (#size DebugInfo)
    alignment _ = alignment (undefined :: CInt) -- TODO (I just guessed)

    peek ptr = do
      startLine    <- #{peek DebugInfo, sline } ptr
      startCol     <- #{peek DebugInfo, scol  } ptr
      endLine      <- #{peek DebugInfo, eline } ptr
      endCol       <- #{peek DebugInfo, ecol  } ptr
      fileName     <- #{peek DebugInfo, file  } ptr >>= peekCString
      functionName <- #{peek DebugInfo, name  } ptr >>= peekCString
      return LocationInfo {..}

    poke ptr (LocationInfo{..}) =
        error "Sorry, we're not really Storable, just use it for peek :("

data DwarfUnit
data Instruction

peekDwarfUnitName :: Ptr DwarfUnit -> IO CString
peekDwarfUnitName ptr = #{peek PublicDwarfUnit, name } ptr

showExecutionStack :: ExecutionStack -> String
showExecutionStack stack =
    "Stack trace:\n" ++
    concatMap display ([0..] `zip` units)
  where
    units = unsafePerformIO $ mapM getStackUnit (stackIndexes stack)
    display (ix, trace) = unlines $ zipWith ($) formatters strings
      where formatters = (printf "%4u: %s" (ix :: Int)) : repeat ("      " ++)
            strings    = prepareStackUnit trace

-- | Reify the stack. This is the only way to get an ExecutionStack value.
currentExecutionStack :: IO (ExecutionStack)
currentExecutionStack = IO (\s -> let (## new_s, byteArray## ##) = reifyStack## s
                                      ba = ExecutionStack byteArray##
                                  in (## new_s, ba ##) )

-- | Pretty print the stack. Will print it to stdout. Note that this is
-- more efficent than doing 'print' as no intermediete Haskell values will
-- get created
dumpExecutionStack :: ExecutionStack -> IO ()
dumpExecutionStack (ExecutionStack ba) = IO (\s -> let new_s = dumpStack## ba s
                                                   in (## new_s, () ##))

-- | Initialize Dwarf Memory. There's no need to call this, as the
-- functions themselves call this. Safe to call twice
foreign import ccall "Dwarf.h dwarf_ensure_init" dwarfInit :: IO ()
  -- Warning, dwarf_init() is something else! Its exported in libdwarf

-- | Free the memory allocated when doing 'dwarfInit'. This module doesn't
-- automatically free the memory. Instead it will hang around for the whole
-- program execution once it's initialized.
--
-- Safe to call twice.
-- 
-- Be careful! The CStrings in 'Locationinfo' will become invalidated!
foreign import ccall "dwarf_free" dwarfFree :: IO ()

-- For the given instruction pointer, how many LocationInfos does it have?
foreign import ccall "Dwarf.h dwarf_addr_num_infos"
    dwarfAddrNumInfos :: Ptr Instruction -> IO CInt

--  StgWord dwarf_lookup_ip(void *ip, DwarfUnit** p_unit, DebugInfo *infos, int max_num_infos)
foreign import ccall "Dwarf.h dwarf_lookup_ip"
  dwarfLookupIpForeign :: 
       Ptr Instruction -- ^ Instruction Pointer
    -> Ptr (Ptr DwarfUnit) -- ^ DwarfUnit Pointer Pointer
    -> Ptr LocationInfo -- ^ LocationInfos to write
    -> CInt -- ^ Max amount of LocationInfo one can write
    -> IO CInt -- ^ How many LocationInfos was actually written

getStackUnitCustom :: 
       Ptr Instruction -- ^ Instruction Pointer
    -> Int -- ^ Max amount to write
    -> IO StackUnit -- ^ Result
getStackUnitCustom ip maxNumInfos = do
    alloca $ \ppDwarfUnit ->
      allocaArray maxNumInfos $ \infos -> do
        numWritten <- dwarfLookupIpForeign ip ppDwarfUnit infos cMaxNumInfos
        pDwarfUnit <- peek ppDwarfUnit
        unitName <- peekDwarfUnitName pDwarfUnit >>= peekCString
        procedureName <- return $ (error "D'oh, also have to get a DwarfProc") pDwarfUnit
        locationInfos <- mapM (peekElemOff infos)
                              [0..(fromIntegral numWritten)-1 :: Int]
        return StackUnit{..}
  where
    cMaxNumInfos = fromIntegral maxNumInfos

getStackUnit ::
       Ptr Instruction
    -> IO StackUnit
getStackUnit ip = dwarfAddrNumInfos ip >>= (getStackUnitCustom ip . fromIntegral)

getStackUnits ::
       ExecutionStack
    -> IO [StackUnit]
getStackUnits = mapM getStackUnit . stackIndexes
