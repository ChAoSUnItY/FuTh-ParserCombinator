import Control.Applicative
  ( Alternative
      ( many,
        some
      ),
  )
import Data.Char
  ( isAlpha,
    isNumber,
  )

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }
