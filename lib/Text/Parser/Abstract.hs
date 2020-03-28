module Text.Parser.Abstract where

import Language.Syntactic

data Syntax a where
    SynText :: Text -> Syntax Text
    ...


