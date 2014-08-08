-----------------------------------------------------------------------------
-- |
-- Module      :  GHCJS.TypedArray
-- Copyright   :  C. A. McCann 2014
--
-- Maintainer  :  cam@uptoisomorphism.net
-- Stability   :  double bonus experimental deluxe
-- Portability :  GHCJS only
--
-- Some slightly over-complicated bindings hacked together for working with
-- JavaScript Typed Arrays. This is intended only for use with FFI bindings
-- that make heavy use of Typed Arrays, primarily WebGL. For anything else,
-- you're better off using standard Haskell types.
--
-- The eventual goal is to reformulate this to interoperate nicely with e.g.
-- 'Data.Vector.Storable' but there was a bug with vectors of 'Float' which 
-- was a show-stopper for WebGL purposes and I'm not sure how to correctly wrap 
-- a TypedArray created externally into a Vector...
-- 

module GHCJS.TypedArray (
    -- * Buffer types 
    ArrayBuffer', ArrayBuffer,
    ArrayBufferView(..),
    TypedArray', TypedArray,
    BufferDataView', BufferDataView,
    
    -- * Element type classes
    DataViewElement(..),
    TypedArrayElement(..),
    
    -- * Various other functions
    byteLength, byteOffset, arrayBuffer, arrayLength
    ) where

import Control.Applicative
import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHCJS.Types (JSRef, JSArray)
import GHCJS.Foreign (toArray)
import GHCJS.Marshal (ToJSRef(..), FromJSRef(..))

-- | An 'ArrayBuffer' is an opaque chunk of binary data. It represents the underlying
--   mutable storage for the other types here.
type ArrayBuffer = JSRef ArrayBuffer'
data ArrayBuffer'

-- | 'ArrayBufferView' is the general form of typed arrays, and allows access to
--   the elements of an 'ArrayBuffer'. 
newtype ArrayBufferView a = ArrayBufferView { unArrayBufferView :: JSRef (ArrayBufferView a) }

-- | 'TypedArray' represents an 'ArrayBufferView'
--   used for a specific data type.
type TypedArray a = ArrayBufferView (TypedArray' a)
data TypedArray' a

-- | 'BufferDataView' allows heterogenous access to the contents of an 
--   'ArrayBuffer', such as for reading binary data formats.

type BufferDataView = ArrayBufferView BufferDataView'
data BufferDataView'

-- | The class of types which can be read from a general DataView. These are not
--   inherently dependent on JavaScript support so other types can be added here
--   if desired.
--
--   This class has basically the same purpose and meaning as 'Foreign.Storable'.
class DataViewElement a where
    -- | Read an element of a BufferDataView at a given offset
    readDataViewEndian  :: BufferDataView  -- ^ The DataView to read from
                        -> Int             -- ^ The byte offset of the element
                        -> Bool            -- ^ Flag indicating whether to read as little-endian
                        -> IO a
    
    -- | write an element to a BufferDataView at a given offset
    writeDataViewEndian :: BufferDataView  -- ^ The DataView to write to
                        -> Int             -- ^ The byte offset to write at
                        -> a               -- ^ The value to be written
                        -> Bool            -- ^ Flag indicating whether to write as little-endian
                        -> IO ()
    
    -- | Read an element of a BufferDataView at a given offset. Uses big-endian 
    --   byte order, to match the default behavior of Data.Binary.
    readDataView :: BufferDataView -> Int -> IO a
    readDataView b i = readDataViewEndian b i False
    
    -- | Write an element to a BufferDataView at a given offset. Uses big-endian 
    --   byte order, to match the default behavior of Data.Binary.
    writeDataView :: BufferDataView -> Int -> a -> IO ()
    writeDataView b i x = writeDataViewEndian b i x False

-- | The class of types for which a 'TypedArray' class exists in JavaScript.
class (ToJSRef a, FromJSRef a, DataViewElement a) => TypedArrayElement a where 
    -- | Create a new 'TypedArray' with the given size
    newArray :: Int -> IO (TypedArray a)
    
    -- | Convert a standard JavaScript array into a 'TypedArray'
    fromJSArray :: JSArray a -> IO (TypedArray a)
    
    -- | Create a copy of a 'TypedArray'.
    cloneArray :: TypedArray a -> IO (TypedArray a)
    
    -- | Use the provided 'ArrayBuffer' as the backing data for a 'TypedArray'. Note 
    --   that changes to the resulting 'TypedArray' will be visible in other 'TypedArrray'
    --   objects created with the same 'ArrayBuffer', even if their types differ.
    fromArrayBuffer :: ArrayBuffer -> IO (TypedArray a)
    
    -- | Like 'fromArrayBuffer', but with a byte offset and length specifying 
    --   how much of the 'ArrayBuffer' to use.
    fromArrayBufferSlice :: ArrayBuffer -> Int -> Int -> IO (TypedArray a)
    
    -- | Create a 'TypedArray' from a Haskell list. By default this conversion is
    --   done going via a JavaScript array.
    listToArray :: [a] -> IO (TypedArray a)
    listToArray = fromJSArray <=< toArray <=< mapM toJSRef
    
    -- | Convert a 'TypedArray' into a Haskell list.
    arrayToList :: TypedArray a -> IO [a]
    arrayToList arr = fmap fromJust <$> mapM (fromJSRef <=< js_readArray arr) [0..arrayLength arr - 1]
    
    -- | Read the 'TypedArray' element at the given index. 
    readArray :: TypedArray a -> Int -> IO a
    readArray arr ix = fmap fromJust $ fromJSRef =<< js_readArray arr ix
    
    -- | Write a value to the 'TypedArray' at the given index. 
    writeArray :: TypedArray a -> Int -> a -> IO ()
    writeArray arr ix val = js_writeArray arr ix =<< toJSRef val


instance DataViewElement Int8 where 
    readDataViewEndian b i _ = getViewInt8 b i
    writeDataViewEndian b x i _ = setViewInt8 b x i
instance TypedArrayElement Int8 where 
    newArray = newInt8Array
    cloneArray = copyInt8Array
    fromJSArray = toInt8Array
    fromArrayBuffer = abToInt8Array
    fromArrayBufferSlice = abSliceToInt8Array
instance DataViewElement Word8 where 
    readDataViewEndian b i _ = getViewWord8 b i
    writeDataViewEndian b x i _ = setViewWord8 b x i
instance TypedArrayElement Word8 where 
    newArray = newWord8Array
    cloneArray = copyWord8Array
    fromJSArray = toWord8Array
    fromArrayBuffer = abToWord8Array
    fromArrayBufferSlice = abSliceToWord8Array
instance DataViewElement Int16 where 
    readDataViewEndian = getViewInt16
    writeDataViewEndian = setViewInt16
instance TypedArrayElement Int16 where 
    newArray = newInt16Array
    cloneArray = copyInt16Array
    fromJSArray = toInt16Array
    fromArrayBuffer = abToInt16Array
    fromArrayBufferSlice = abSliceToInt16Array
instance DataViewElement Word16 where 
    readDataViewEndian = getViewWord16
    writeDataViewEndian = setViewWord16
instance TypedArrayElement Word16 where 
    newArray = newWord16Array
    cloneArray = copyWord16Array
    fromJSArray = toWord16Array
    fromArrayBuffer = abToWord16Array
    fromArrayBufferSlice = abSliceToWord16Array
instance DataViewElement Int32 where 
    readDataViewEndian = getViewInt32
    writeDataViewEndian = setViewInt32
instance TypedArrayElement Int32 where 
    newArray = newInt32Array
    cloneArray = copyInt32Array
    fromJSArray = toInt32Array
    fromArrayBuffer = abToInt32Array
    fromArrayBufferSlice = abSliceToInt32Array
instance DataViewElement Word32 where 
    readDataViewEndian = getViewWord32
    writeDataViewEndian = setViewWord32
instance TypedArrayElement Word32 where 
    newArray = newWord32Array
    cloneArray = copyWord32Array
    fromJSArray = toWord32Array
    fromArrayBuffer = abToWord32Array
    fromArrayBufferSlice = abSliceToWord32Array
instance DataViewElement Float where 
    readDataViewEndian = getViewFloat32
    writeDataViewEndian = setViewFloat32
instance TypedArrayElement Float where 
    newArray = newFloat32Array
    cloneArray = copyFloat32Array
    fromJSArray = toFloat32Array
    fromArrayBuffer = abToFloat32Array
    fromArrayBufferSlice = abSliceToFloat32Array
instance DataViewElement Double where 
    readDataViewEndian = getViewFloat64
    writeDataViewEndian = setViewFloat64
instance TypedArrayElement Double where 
    newArray = newFloat64Array
    cloneArray = copyFloat64Array
    fromJSArray = toFloat64Array
    fromArrayBuffer = abToFloat64Array
    fromArrayBufferSlice = abSliceToFloat64Array

instance DataViewElement Word64 where
    readDataViewEndian b i isLE = do
        w1 <- getViewWord32 b (i + d1) isLE
        w2 <- getViewWord32 b (i + d2) isLE
        return $ fromIntegral w1 * 2^32 + fromIntegral w2
      where (d1, d2) = if isLE then (1, 0) else (0, 1)
    writeDataViewEndian b i x isLE = do 
        setViewWord32 b (i + d1) (fromIntegral w1) isLE
        setViewWord32 b (i + d2) (fromIntegral w2) isLE
      where (d1, d2) = if isLE then (1, 0) else (0, 1)
            (w1, w2) = quotRem x (2^32)

instance DataViewElement Int64 where
    readDataViewEndian b i isLE = do
        w1 <- getViewInt32 b (i + d1) isLE
        w2 <- getViewInt32 b (i + d2) isLE
        return $ fromIntegral w1 * 2^32 + fromIntegral w2
      where (d1, d2) = if isLE then (1, 0) else (0, 1)
    writeDataViewEndian b i x isLE = do 
        setViewInt32 b (i + d1) (fromIntegral w1) isLE
        setViewInt32 b (i + d2) (fromIntegral w2) isLE
      where (d1, d2) = if isLE then (1, 0) else (0, 1)
            (w1, w2) = quotRem x (2^32)

foreign import javascript unsafe "new ArrayBuffer($1)" newArrayBuffer :: Int -> IO ArrayBuffer

foreign import javascript unsafe "new Int8Array($1)" newInt8Array :: Int -> IO (TypedArray Int8)
foreign import javascript unsafe "new Uint8Array($1)" newWord8Array :: Int -> IO (TypedArray Word8)
foreign import javascript unsafe "new Int16Array($1)" newInt16Array :: Int -> IO (TypedArray Int16)
foreign import javascript unsafe "new Uint16Array($1)" newWord16Array :: Int -> IO (TypedArray Word16)
foreign import javascript unsafe "new Int32Array($1)" newInt32Array :: Int -> IO (TypedArray Int32)
foreign import javascript unsafe "new Uint32Array($1)" newWord32Array :: Int -> IO (TypedArray Word32)
foreign import javascript unsafe "new Float32Array($1)" newFloat32Array :: Int -> IO (TypedArray Float)
foreign import javascript unsafe "new Float64Array($1)" newFloat64Array :: Int -> IO (TypedArray Double)

foreign import javascript unsafe "new Int8Array($1)" toInt8Array :: JSArray Int8 -> IO (TypedArray Int8)
foreign import javascript unsafe "new Uint8Array($1)" toWord8Array :: JSArray Word8 -> IO (TypedArray Word8)
foreign import javascript unsafe "new Int16Array($1)" toInt16Array :: JSArray Int16 -> IO (TypedArray Int16)
foreign import javascript unsafe "new Uint16Array($1)" toWord16Array :: JSArray Word16 -> IO (TypedArray Word16)
foreign import javascript unsafe "new Int32Array($1)" toInt32Array :: JSArray Int32 -> IO (TypedArray Int32)
foreign import javascript unsafe "new Uint32Array($1)" toWord32Array :: JSArray Word32 -> IO (TypedArray Word32)
foreign import javascript unsafe "new Float32Array($1)" toFloat32Array :: JSArray Float -> IO (TypedArray Float)
foreign import javascript unsafe "new Float64Array($1)" toFloat64Array :: JSArray Double -> IO (TypedArray Double)

foreign import javascript unsafe "new Int8Array($1)" copyInt8Array :: TypedArray Int8 -> IO (TypedArray Int8)
foreign import javascript unsafe "new Uint8Array($1)" copyWord8Array :: TypedArray Word8 -> IO (TypedArray Word8)
foreign import javascript unsafe "new Int16Array($1)" copyInt16Array :: TypedArray Int16 -> IO (TypedArray Int16)
foreign import javascript unsafe "new Uint16Array($1)" copyWord16Array :: TypedArray Word16 -> IO (TypedArray Word16)
foreign import javascript unsafe "new Int32Array($1)" copyInt32Array :: TypedArray Int32 -> IO (TypedArray Int32)
foreign import javascript unsafe "new Uint32Array($1)" copyWord32Array :: TypedArray Word32 -> IO (TypedArray Word32)
foreign import javascript unsafe "new Float32Array($1)" copyFloat32Array :: TypedArray Float -> IO (TypedArray Float)
foreign import javascript unsafe "new Float64Array($1)" copyFloat64Array :: TypedArray Double -> IO (TypedArray Double)

foreign import javascript unsafe "new Int8Array($1)" abToInt8Array :: ArrayBuffer -> IO (TypedArray Int8)
foreign import javascript unsafe "new Uint8Array($1)" abToWord8Array :: ArrayBuffer -> IO (TypedArray Word8)
foreign import javascript unsafe "new Int16Array($1)" abToInt16Array :: ArrayBuffer -> IO (TypedArray Int16)
foreign import javascript unsafe "new Uint16Array($1)" abToWord16Array :: ArrayBuffer -> IO (TypedArray Word16)
foreign import javascript unsafe "new Int32Array($1)" abToInt32Array :: ArrayBuffer -> IO (TypedArray Int32)
foreign import javascript unsafe "new Uint32Array($1)" abToWord32Array :: ArrayBuffer -> IO (TypedArray Word32)
foreign import javascript unsafe "new Float32Array($1)" abToFloat32Array :: ArrayBuffer -> IO (TypedArray Float)
foreign import javascript unsafe "new Float64Array($1)" abToFloat64Array :: ArrayBuffer -> IO (TypedArray Double)

foreign import javascript unsafe "new Int8Array($1, $2, $3)" abSliceToInt8Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Int8)
foreign import javascript unsafe "new Uint8Array($1, $2, $3)" abSliceToWord8Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Word8)
foreign import javascript unsafe "new Int16Array($1, $2, $3)" abSliceToInt16Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Int16)
foreign import javascript unsafe "new Uint16Array($1, $2, $3)" abSliceToWord16Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Word16)
foreign import javascript unsafe "new Int32Array($1, $2, $3)" abSliceToInt32Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Int32)
foreign import javascript unsafe "new Uint32Array($1, $2, $3)" abSliceToWord32Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Word32)
foreign import javascript unsafe "new Float32Array($1, $2, $3)" abSliceToFloat32Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Float)
foreign import javascript unsafe "new Float64Array($1, $2, $3)" abSliceToFloat64Array :: ArrayBuffer -> Int -> Int -> IO (TypedArray Double)

foreign import javascript unsafe "new DataView($1)" newDataView :: ArrayBuffer -> IO BufferDataView
foreign import javascript unsafe "new DataView($1, $2, $3)" newSliceDataView :: ArrayBuffer -> Int -> Int -> IO BufferDataView

foreign import javascript unsafe "$1['getInt8']($2)" getViewInt8 :: BufferDataView -> Int -> IO Int8
foreign import javascript unsafe "$1['getInt16']($2, $3)" getViewInt16 :: BufferDataView -> Int -> Bool -> IO Int16
foreign import javascript unsafe "$1['getInt32']($2, $3)" getViewInt32 :: BufferDataView -> Int -> Bool -> IO Int32
foreign import javascript unsafe "$1['getUint8']($2)" getViewWord8 :: BufferDataView -> Int -> IO Word8
foreign import javascript unsafe "$1['getUint16']($2, $3)" getViewWord16 :: BufferDataView -> Int -> Bool -> IO Word16
foreign import javascript unsafe "$1['getUint32']($2, $3)" getViewWord32 :: BufferDataView -> Int -> Bool -> IO Word32
foreign import javascript unsafe "$1['getFloat32']($2, $3)" getViewFloat32 :: BufferDataView -> Int -> Bool -> IO Float
foreign import javascript unsafe "$1['getFloat64']($2, $3)" getViewFloat64 :: BufferDataView -> Int -> Bool -> IO Double

foreign import javascript unsafe "$1['setInt8']($2, $3)" setViewInt8 :: BufferDataView -> Int -> Int8 -> IO ()
foreign import javascript unsafe "$1['setInt16']($2, $3, $4)" setViewInt16 :: BufferDataView -> Int -> Int16 -> Bool -> IO ()
foreign import javascript unsafe "$1['setInt32']($2, $3, $4)" setViewInt32 :: BufferDataView -> Int -> Int32 -> Bool -> IO ()
foreign import javascript unsafe "$1['setUint8']($2, $3)" setViewWord8 :: BufferDataView -> Int -> Word8 -> IO ()
foreign import javascript unsafe "$1['setUint16']($2, $3, $4)" setViewWord16 :: BufferDataView -> Int -> Word16 -> Bool -> IO ()
foreign import javascript unsafe "$1['setUint32']($2, $3, $4)" setViewWord32 :: BufferDataView -> Int -> Word32 -> Bool -> IO ()
foreign import javascript unsafe "$1['setFloat32']($2, $3, $4)" setViewFloat32 :: BufferDataView -> Int -> Float -> Bool -> IO ()
foreign import javascript unsafe "$1['setFloat64']($2, $3, $4)" setViewFloat64 :: BufferDataView -> Int -> Double -> Bool -> IO ()


-- | Get the length in bytes of a TypedArray's view into its buffer.
foreign import javascript unsafe "$1['byteLength']"
    byteLength :: ArrayBufferView a -> Int

-- | Get the starting offset of a TypedArray's view into its buffer.
foreign import javascript unsafe "$1['byteOffset']"
    byteOffset :: ArrayBufferView a -> Int

-- | Get the storage buffer referenced by a 'TypedArray'.
foreign import javascript unsafe "$1['buffer']"
    arrayBuffer :: ArrayBufferView a -> ArrayBuffer

-- | Get the length in elements of a 'TypedArray'.
foreign import javascript unsafe "$1.length"
    arrayLength :: TypedArray a -> Int

foreign import javascript unsafe "$1[$2]"
    js_readArray :: TypedArray a -> Int -> IO (JSRef a)

foreign import javascript unsafe "$1[$2] = $3;"
    js_writeArray :: TypedArray a -> Int -> JSRef a -> IO ()

