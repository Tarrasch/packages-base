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
  , dumpExecutionStack
  -- * Complicated interface
  -- ** ExecutionStack
  , ExecutionStack ()
  , stackSize
  , stackIndex
  , stackIndices
  -- ** Structures the stack can translate to
  , StackFrame(..)
  , LocationInfo(..)
  -- ** Get StackFrames
  , getStackFrames
  , getStackFrame
  , getStackFrameCustom
  -- ** Other
  , dwarfInit
  , dwarfFree
  ) where

import GHC.IO (IO(..)
             , unsafePerformIO)
import GHC.Exts
import GHC.Word (Word16)
import Foreign.C.String (peekCString, CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (alloca, allocaArray)
import Text.Printf (printf)

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

stackIndices :: ExecutionStack -> [Ptr Instruction]
stackIndices stack = map (stackIndex stack) [0..(stackSize stack)-1]


data StackFrame = StackFrame {
    unitName      :: !String
  , procedureName :: !String
  , locationInfos :: ![LocationInfo] -- ^ Empty without @-g@ flag to @ghc@
  }
  -- Looking at Dwarf.h, this is one DwarfUnit and many DebugInfos.

instance Show StackFrame where
    show = showStackFrame

-- TODO: Better name than prepareStackFrame?

-- | Like 'show', without @unlines@
prepareStackFrame :: StackFrame -> [String]
prepareStackFrame su | null (locationInfos su) = (:[]) $
        unitName su ++
        " (using " ++ unitName su ++ ")"
    --  mySrcFun (using /path/lib.so)
prepareStackFrame su | otherwise = map showLocationInfo $ locationInfos su

showStackFrame :: StackFrame -> String
showStackFrame = unlines . prepareStackFrame

-- | Location in source code.
data LocationInfo = LocationInfo {
           startLine    :: !Word16,
           startCol     :: !Word16,
           endLine      :: !Word16,
           endCol       :: !Word16,
           fileName     :: !String,
           functionName :: !String
           }
           deriving(Eq)
  -- This struct. Matches the C struct @DebugInfo_@, from dwarf.h

instance Show LocationInfo where
    show = showLocationInfo

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

-- We use these three guys to get type-safety
data DwarfUnit
data DwarfProc
data Instruction

peekDwarfUnitName :: Ptr DwarfUnit -> IO CString
peekDwarfUnitName ptr = #{peek struct DwarfUnit_, name } ptr

peekDwarfProcName :: Ptr DwarfProc -> IO CString
peekDwarfProcName ptr = #{peek struct DwarfProc_, name } ptr

showExecutionStack :: ExecutionStack -> String
showExecutionStack stack =
    "Stack trace:\n" ++
    concatMap display ([0..] `zip` units)
  where
    units = unsafePerformIO $ mapM getStackFrame (stackIndices stack)
    display (ix, trace) = unlines $ zipWith ($) formatters strings
      where formatters = (printf "%4u: %s" (ix :: Int)) : repeat ("      " ++)
            strings    = prepareStackFrame trace

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
-- program execution once it's initialized. Safe to call twice.
foreign import ccall "dwarf_free" dwarfFree :: IO ()

-- For the given instruction pointer, how many LocationInfos does it have?
foreign import ccall "Dwarf.h dwarf_addr_num_infos"
    dwarfAddrNumInfos :: Ptr Instruction -> IO CInt

foreign import ccall "Dwarf.h dwarf_lookup_ip"
  dwarfLookupIp ::
       Ptr Instruction -- ^ Instruction Pointer
    -> Ptr (Ptr DwarfProc) -- ^ DwarfUnit Pointer Pointer
    -> Ptr (Ptr DwarfUnit) -- ^ DwarfUnit Pointer Pointer
    -> Ptr LocationInfo -- ^ LocationInfos to write
    -> CInt -- ^ Max amount of LocationInfo one can write
    -> IO CInt -- ^ How many LocationInfos was actually written


getStackFrameCustom ::
       Ptr Instruction -- ^ Instruction Pointer
    -> Int -- ^ Max amount to write
    -> IO StackFrame -- ^ Result
getStackFrameCustom ip maxNumInfos = do
    alloca $ \ppDwarfProc -> do
      poke ppDwarfProc nullPtr
      alloca $ \ppDwarfUnit ->
        allocaArray maxNumInfos $ \infos -> do
          numWritten <- dwarfLookupIp ip ppDwarfProc ppDwarfUnit infos cMaxNumInfos
          pDwarfProc <- peek ppDwarfProc
          pDwarfUnit <- peek ppDwarfUnit
          unitName <- stringPeekWith peekDwarfUnitName pDwarfUnit
          procedureName <- stringPeekWith peekDwarfProcName pDwarfProc
          locationInfos <- mapM (peekElemOff infos)
                                [0..(fromIntegral numWritten)-1 :: Int]
          return StackFrame{..}
  where
    cMaxNumInfos = fromIntegral maxNumInfos

stringPeekWith :: (Ptr a -> IO CString) -> Ptr a -> IO String
stringPeekWith peeker ptr | ptr == nullPtr = return "<Data not found>"
stringPeekWith peeker ptr | otherwise      = peeker ptr >>= peekCString

getStackFrame ::
       Ptr Instruction
    -> IO StackFrame
getStackFrame ip = dwarfAddrNumInfos ip >>= (getStackFrameCustom ip . fromIntegral)

getStackFrames ::
       ExecutionStack
    -> IO [StackFrame]
getStackFrames = mapM getStackFrame . stackIndices
