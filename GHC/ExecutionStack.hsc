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
--      stack <- reifyStack
--      dumpStack stack
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
    reifyStack
  , dumpStack
  -- * Complicated interface
  -- ** ExecutionStack
  , ExecutionStack ()
  , stackSize
  , stackIndex
  -- ** Structures the stack translates to
  , LocationInfo(..)
  , showLocationInfo
  , StackUnit(..)
  , showStackUnit
  -- * New stuff
  , dwarfLookupPtr
  , dwarfLookupAllPtr 
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

-- We include MachDeps for the constants containing the sizes of values
#include "Rts.h"

data ExecutionStack = ExecutionStack
    { unExecutionStack ::  ByteArray##
    }

instance Show ExecutionStack where
    show = showExecutionStack

-- TODO: Better name?
data StackUnit = StackUnit {
    unitName :: CString
  , procedureName :: CString
  , locationInfos :: [LocationInfo]
  }
  -- Looking at Dwarf.h, this is one DwarfUnit and many DebugInfos.

showStackUnit :: StackUnit -> String
showStackUnit su | null (locationInfos su) =
        -- showCString (unitName su) ++
        "???" ++
        " (using " ++ showCString (unitName su) ++ ")"
    --  mySrcFun (using /path/lib.so)
showStackUnit su | otherwise = unlines $ map showLocationInfo $ locationInfos su

-- | This is a candidate for a Instruction Pointe.  This struct
-- matches the C struct @DebugInfo_@, from dwarf.h
data LocationInfo = LocationInfo {
           startLine    :: !Word16,
           startCol     :: !Word16,
           endLine      :: !Word16,
           endCol       :: !Word16,
           fileName     :: !CString,
           functionName :: !CString
           }
           deriving(Eq)

-- | Will only work if there have not been any 'dwarfFree' since the value
-- was created. For this reason 'LocationInfo' have no Show instance.
showLocationInfo :: LocationInfo -> String
showLocationInfo LocationInfo{..} =
    showCString functionName ++
    " (at " ++ showCString fileName ++
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
      fileName     <- #{peek DebugInfo, file  } ptr
      functionName <- #{peek DebugInfo, name  } ptr
      return LocationInfo {..}

    poke ptr (LocationInfo{..}) = do
      #{poke DebugInfo, sline } ptr startLine
      #{poke DebugInfo, scol  } ptr startCol
      #{poke DebugInfo, eline } ptr endLine
      #{poke DebugInfo, ecol  } ptr endCol
      #{poke DebugInfo, file  } ptr fileName
      #{poke DebugInfo, name  } ptr functionName
      -- Won't need poke. We include it for completeness

data DwarfUnit

peekDwarfUnitName :: Ptr DwarfUnit -> IO CString
peekDwarfUnitName ptr = #{peek PublicDwarfUnit, name } ptr

showExecutionStack :: ExecutionStack -> String
showExecutionStack stack =
    "Stack trace:\n" ++
    concatMap display ([1..] `zip` traces)
  where
    traces = error "TODO"
    display (ix, trace) = printf "%4u: " (ix :: Int) ++ displayTrace trace ++ "\n"
    displayTrace (Just li)  = showLocationInfo li
    displayTrace Nothing    = "... unknown ..."

-- | Reify the stack. This is the only way to get an ExecutionStack value.
reifyStack :: IO (ExecutionStack)
reifyStack = IO (\s -> let (## new_s, byteArray## ##) = reifyStack## s
                           ba = ExecutionStack byteArray##
                       in (## new_s, ba ##) )

-- | Pretty print the stack. Will print it to stdout. Note that this is
-- more efficent than doing 'print' as no intermediete Haskell values will
-- get created
dumpStack :: ExecutionStack -> IO ()
dumpStack (ExecutionStack ba) = IO (\s -> let new_s = dumpStack## ba s
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
    dwarfAddrNumInfos :: Ptr () -> IO CInt

--  StgWord dwarf_lookup_ip(void *ip, DwarfUnit** p_unit, DebugInfo *infos, int max_num_infos)
foreign import ccall "Dwarf.h dwarf_lookup_ip"
  dwarfLookupIpForeign :: 
       Ptr () -- ^ Instruction Pointer
    -> Ptr (Ptr DwarfUnit) -- ^ DwarfUnit Pointer Pointer
    -> Ptr LocationInfo -- ^ LocationInfos to write
    -> CInt -- ^ Max amount of LocationInfo one can write
    -> IO CInt -- ^ How many LocationInfos was actually written

dwarfLookupPtr :: 
       Ptr () -- ^ Instruction Pointer
    -> Int -- ^ Max amount to write
    -> IO StackUnit -- ^ Result
dwarfLookupPtr ip maxNumInfos = do
    alloca $ \ppDwarfUnit ->
      allocaArray maxNumInfos $ \infos -> do
        numWritten <- dwarfLookupIpForeign ip ppDwarfUnit infos cMaxNumInfos
        pDwarfUnit <- peek ppDwarfUnit
        unitName <- peekDwarfUnitName pDwarfUnit
        procedureName <- return $ (error "D'oh, also have to get a DwarfProc") pDwarfUnit
        locationInfos <- mapM (peekElemOff infos)
                              [0..(fromIntegral numWritten)-1 :: Int]
        return StackUnit{..}
  where
    cMaxNumInfos = fromIntegral maxNumInfos

dwarfLookupAllPtr ::
       Ptr ()
    -> IO StackUnit
dwarfLookupAllPtr ip = dwarfAddrNumInfos ip >>= (dwarfLookupPtr ip . fromIntegral)

-- | The number of functions on your stack
stackSize :: ExecutionStack -> Int
stackSize stack =
    I## (sizeofByteArray## (unExecutionStack stack)) `div` (#const SIZEOF_HSPTR)

stackIndex :: ExecutionStack -> Int -> Addr##
stackIndex (ExecutionStack ba##) (I## i##) = indexAddrArray## ba## i##

showCString :: CString -> String
showCString = unsafePerformIO . peekCString
  -- It's safe to read here oftentimes in this module.  The c-strings are
  -- allocated on the actual C heap, which is not handled by the rts.
