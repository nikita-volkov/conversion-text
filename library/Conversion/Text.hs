{-# LANGUAGE CPP #-}
module Conversion.Text () where

import BasePrelude
import Conversion
import Conversion.ByteString
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB


instance Conversion TS.Text String where
  {-# INLINE convert #-}
  convert = TS.unpack

instance Conversion TS.Text TL.Text where
  {-# INLINE convert #-}
  convert = TL.fromStrict

instance Conversion TS.Text TLB.Builder where
  {-# INLINE convert #-}
  convert = TLB.fromText

instance Conversion TS.Text BS.ByteString where
  {-# INLINE convert #-}
  convert = TE.encodeUtf8

instance Conversion TS.Text BL.ByteString where
  {-# INLINE convert #-}
  convert = BL.fromStrict . TE.encodeUtf8

instance Conversion TS.Text BB.Builder where
  {-# INLINE convert #-}
#if MIN_VERSION_text(1,2,0)
  convert = TE.encodeUtf8Builder
#else
  convert = convert . TE.encodeUtf8
#endif


instance Conversion TL.Text String where
  {-# INLINE convert #-}
  convert = TL.unpack

instance Conversion TL.Text TS.Text where
  {-# INLINE convert #-}
  convert = TL.toStrict

instance Conversion TL.Text TLB.Builder where
  {-# INLINE convert #-}
  convert = TLB.fromLazyText

instance Conversion TL.Text BS.ByteString where
  {-# INLINE convert #-}
  convert = convert . TLE.encodeUtf8

instance Conversion TL.Text BL.ByteString where
  {-# INLINE convert #-}
  convert = TLE.encodeUtf8

instance Conversion TL.Text BB.Builder where
  {-# INLINE convert #-}
#if MIN_VERSION_text(1,2,0)
  convert = TLE.encodeUtf8Builder
#else
  convert = convert . TLE.encodeUtf8
#endif


instance Conversion TLB.Builder String where
  {-# INLINE convert #-}
  convert = convert . TLB.toLazyText

instance Conversion TLB.Builder TS.Text where
  {-# INLINE convert #-}
  convert = convert . TLB.toLazyText

instance Conversion TLB.Builder TL.Text where
  {-# INLINE convert #-}
  convert = TLB.toLazyText

instance Conversion TLB.Builder BS.ByteString where
  {-# INLINE convert #-}
  convert = convert . TLB.toLazyText

instance Conversion TLB.Builder BL.ByteString where
  {-# INLINE convert #-}
  convert = convert . TLB.toLazyText

instance Conversion TLB.Builder BB.Builder where
  {-# INLINE convert #-}
  convert = convert . TLB.toLazyText


instance Conversion BS.ByteString (Either TEE.UnicodeException TS.Text) where
  {-# INLINE convert #-}
  convert = TE.decodeUtf8'

instance Conversion BS.ByteString (Either TEE.UnicodeException TL.Text) where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8' . convert

instance Alternative f => Conversion BS.ByteString (f TS.Text) where
  {-# INLINE convert #-}
  convert = convert . TE.decodeUtf8'

instance Alternative f => Conversion BS.ByteString (f TL.Text) where
  {-# INLINE convert #-}
  convert = convert . TLE.decodeUtf8' . convert

instance Alternative f => Conversion BS.ByteString (f TLB.Builder) where
  {-# INLINE convert #-}
  convert = fmap TLB.fromLazyText . convert


instance Conversion BL.ByteString (Either TEE.UnicodeException TS.Text) where
  {-# INLINE convert #-}
  convert = fmap convert . TLE.decodeUtf8'

instance Conversion BL.ByteString (Either TEE.UnicodeException TL.Text) where
  {-# INLINE convert #-}
  convert = TLE.decodeUtf8'

instance Alternative f => Conversion BL.ByteString (f TS.Text) where
  {-# INLINE convert #-}
  convert = convert . fmap TL.toStrict . TLE.decodeUtf8'

instance Alternative f => Conversion BL.ByteString (f TL.Text) where
  {-# INLINE convert #-}
  convert = convert . TLE.decodeUtf8'

instance Alternative f => Conversion BL.ByteString (f TLB.Builder) where
  {-# INLINE convert #-}
  convert = fmap TLB.fromLazyText . convert


instance Alternative f => Conversion BB.Builder (f TS.Text) where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Alternative f => Conversion BB.Builder (f TL.Text) where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString

instance Alternative f => Conversion BB.Builder (f TLB.Builder) where
  {-# INLINE convert #-}
  convert = convert . BB.toLazyByteString


instance Conversion String TS.Text where
  {-# INLINE convert #-}
  convert = TS.pack

instance Conversion String TL.Text where
  {-# INLINE convert #-}
  convert = TL.pack

instance Conversion String TLB.Builder where
  {-# INLINE convert #-}
  convert = TLB.fromString


instance Conversion Char TS.Text where
  {-# INLINE convert #-}
  convert = TS.singleton

instance Conversion Char TL.Text where
  {-# INLINE convert #-}
  convert = TL.singleton

instance Conversion Char TLB.Builder where
  {-# INLINE convert #-}
  convert = TLB.singleton
