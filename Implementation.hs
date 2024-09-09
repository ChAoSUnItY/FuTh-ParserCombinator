import Data.Char           (
                             isNumber, 
                             isAlpha
                           )
import Control.Applicative ( 
                             Alternative (
                                some,
                                many
                             ) 
                           )

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}
