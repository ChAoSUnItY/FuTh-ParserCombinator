{-# LANGUAGE LambdaCase #-}

import Control.Applicative
  ( Alternative
      ( empty,
        many,
        some,
        (<|>)
      ),
  )
import Data.Char
  ( isAlpha,
    isNumber,
  )

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }
