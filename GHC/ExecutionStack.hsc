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
  -- ** Retrieve candidates
  , getSimpleTraces
  , getTraces
  , getLocationInfos
  , getNumLocationInfos
  , getAllLocationInfos
  -- ** Other
  , LocationInfo(..)
  , showLocationInfo
  , DwarfUnit(..)
  , dwarfInit
  , dwarfFree
  , dwarfLookupIp
  -- * New stuff
  , dwarfLookupPtr
  , dwarfLookupAllPtr 
  , StackUnit(..)
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
#include "MachDeps.h"
#include "Rts.h"

data ExecutionStack = ExecutionStack
    { unExecutionStack ::  ByteArray##
    }

instance Show ExecutionStack where
    show = showExecutionStack

-- TODO: Better name anyone?
data StackUnit = StackUnit {
    unitName :: CString
  , procedureName :: CString
  , locationInfos :: [LocationInfo]
  }
  -- Looking at Dwarf.h, this is one DwarfUnit and many DebugInfos.

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
    peek functionName ++
    " (at " ++ peek fileName ++
    ":" ++ show startLine ++ ":" ++ show startCol ++
    "-" ++ show endLine ++ ":" ++ show endCol ++
    ")"
    -- example: "bindIO (at libraries/base/GHC/Base.lhs:609:61-609:77)"
  where
    peek :: CString -> String
    peek = unsafePerformIO . peekCString
      -- It's safe to read here since the strings are allocated on the
      -- actual C heap, which is not handled by the rts.

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

data DwarfUnit2

peekDwarfUnitName :: Ptr DwarfUnit2 -> IO CString
peekDwarfUnitName ptr = #{peek PublicDwarfUnit, name } ptr

showExecutionStack :: ExecutionStack -> String
showExecutionStack stack =
    "Stack trace:\n" ++
    concatMap display ([1..] `zip` traces)
  where
    traces = getSimpleTraces stack
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
    -> Ptr (Ptr DwarfUnit2) -- ^ DwarfUnit Pointer Pointer
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

-- | To use this type in the rest of this module is a TODO
data DwarfUnit = DwarfUnit Addr##

-- | Given the address of a function. Collect data about it
dwarfLookupIp :: Addr##
              -> MutableByteArray## RealWorld {-^ Data will be written here.
                                             Can be of any size, if it's
                                             too small nothing will be
                                             written to it. -}
              -> IO (Int, DwarfUnit) -- ^ (maxNumInfos, dwarfUnit)
dwarfLookupIp = undefined
-- dwarfLookupIp addr## mba## = IO $ \s ->
--     let (## s', numWritten##, dwarfUnit## ##) = dwarfLookupIp## addr## mba## s
--     in (## s', (I## numWritten##, DwarfUnit dwarfUnit##) ##)

-- | The number of functions on your stack
stackSize :: ExecutionStack -> Int
stackSize stack =
    I## (sizeofByteArray## (unExecutionStack stack)) `div` (#const SIZEOF_HSPTR)

stackIndex :: ExecutionStack -> Int -> Addr##
stackIndex (ExecutionStack ba##) (I## i##) = indexAddrArray## ba## i##

-- Arash starts to get angry at ##-kinded values..., wish I could use
-- primitive package ...
--
-- Apparently I'm not the first guy, look in
-- libraries/binary/src/Data/Binary/Class.hs
--
-- wrapper for ByteArray## (Arash stole from Class.hs)
data ByteArray = BA   { unBA :: ByteArray## }
data MBA       = MBA  { unMBA :: MutableByteArray## RealWorld }

newByteArray :: Int## -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray## sz s of { (## s', arr ##) ->
  (## s', MBA arr ##) }

freezeByteArray :: MutableByteArray## RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray## arr s of { (## s', arr' ##) ->
  (## s', BA arr' ##) }

-- | Look up the eventual LocationInfos for the functions in the Stack
-- Trace.
getSimpleTraces :: ExecutionStack
                -> [Maybe LocationInfo]
getSimpleTraces stack = map listToMaybe $ getTraces stack 1

-- | Lookup up LocationInfos of a stack with the accuracy provided
getTraces :: ExecutionStack
          -> Int -- ^ Maximum number of traces. Should be >= 1
          -> [[LocationInfo]] -- ^ For each function, its alternative
getTraces stack maxNumInfos = map (\i -> getLocationInfos stack i maxNumInfos)
                                  [0..(stackSize stack)-1]

-- | Given which stack entry, collect a few candidates for the function.
-- The fewer candidates the faster. We've used a heuristic. We guess that
-- the better candidates are always the first elements in the return
-- value.
getLocationInfos :: ExecutionStack
                 -> Int -- ^ Index in the ExecutionStack. Should be in @[0..stackSize)@
                 -> Int -- ^ Maximum number of candidates Should be positive
                 -> [LocationInfo] -- ^ Candidates
getLocationInfos stack i maxNumInfos = infos
    where
        arraySize = maxNumInfos * sizeOf (undefined :: LocationInfo)
        infos = unsafePerformIO $ do
          mba <- newByteArray (unI## arraySize)
          (numInfos, _du) <- dwarfLookupIp (stackIndex stack i) (unMBA mba)
          ba <- freezeByteArray (unMBA mba)
          let getInfo ix = indexLocationInfoArray (unBA ba) ix
          return $ map getInfo [0..(numInfos - 1)]

-- | Calculate the number of candidates of source level functions for an
-- Instruction Pointer
--
-- O(n) time complexity, where n is the result.
getNumLocationInfos :: ExecutionStack
                    -> Int -- ^ Index in the Executionstack. Should be in @[0..stackSize)@
                    -> Int -- ^ Total number of candidates
getNumLocationInfos = error "Ok, this one requires some additional C function to be implemented"

getAllLocationInfos :: ExecutionStack
                    -> Int -- ^ Index in the Executionstack. Should be in @[0..stackSize)@
                    -> [LocationInfo] -- ^ All candidates
getAllLocationInfos = error "Something involving getNumLocationInfos"


indexLocationInfoArray :: ByteArray## -> Int -> LocationInfo
indexLocationInfoArray arr## ix = LocationInfo {..}
  where
    offset = ix * sizeofLocationInfo
    depthIn :: Int -> Int -> Int##
    depthIn sizeInBytes bytesIn = unI## ((offset + bytesIn) `div` sizeInBytes)

    startLine = W16## (indexWord16Array## arr## (depthIn 2 0))
    startCol  = W16## (indexWord16Array## arr## (depthIn 2 2))
    endLine   = W16## (indexWord16Array## arr## (depthIn 2 4))
    endCol    = W16## (indexWord16Array## arr## (depthIn 2 6))

    fileName = Ptr (indexAddrArray## arr## (depthIn (#const SIZEOF_HSPTR) 8))
    functionName = Ptr (
      indexAddrArray## arr## (depthIn (#const SIZEOF_HSPTR) (8 + (#const SIZEOF_HSPTR))))

sizeofLocationInfo :: Int
sizeofLocationInfo =  4*(#const SIZEOF_WORD16) -- {s,e}{line,column}
                    + (#const SIZEOF_HSPTR) -- filename
                    + (#const SIZEOF_HSPTR) -- functionname
                    + (#const SIZEOF_HSWORD) -- depth

unI## :: Int -> Int##
unI## (I## n##) = n##
