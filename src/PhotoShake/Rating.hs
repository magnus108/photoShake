{-# LANGUAGE LambdaCase #-}                                                      
module PhotoShake.Rating
    ( Rating
    , fromString
    ) where


data Rating
    = One
    | Two
    | Three
    | Four
    | Five
    deriving (Show,Eq,Ord)


fromString :: String -> Rating
fromString = \case
    "1" -> One
    "2" -> Two
    "3" -> Three
    "4" -> Four
    "5" -> Five

