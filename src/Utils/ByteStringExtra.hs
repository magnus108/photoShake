module Utils.ByteStringExtra
    ( between
    ) where


import qualified Data.ByteString as BS

between :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
between prefix postfix xs = rating
    where
        (_, after) = BS.breakSubstring prefix xs
        (rating, _) = BS.breakSubstring postfix after
