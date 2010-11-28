module Pretty (
    module Pretty
  , module Text.PrettyPrint
  ) where

import Data.Int (Int8,Int16,Int32,Int64)
import Data.List (intersperse)
import Text.PrettyPrint

pretty :: Pretty a => a -> String
pretty  = render . pp 0

dot :: Doc
dot  = char '.'

commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

commaSep :: Doc -> Doc -> Doc
commaSep a b
  | isEmpty b = a
  | isEmpty a = b
  | otherwise = a <> comma <+> b

ppr :: Pretty a => a -> Doc
ppr  = pp 0

optParens :: Bool -> Doc -> Doc
optParens True = parens
optParens _    = id

optBraces :: Bool -> Doc -> Doc
optBraces True = braces
optBraces _    = id

semis :: [Doc] -> Doc
semis  = foldr step empty
  where
  step d r = d <> semi $+$ r

class Pretty a where
  pp     :: Int -> a -> Doc
  ppList :: Int -> [a] -> Doc
  ppList p as = hsep (map (pp p) as)

instance Pretty Bool where
  pp _ True  = int 1
  pp _ False = int 0

instance Pretty Char where
  pp _ = char
  ppList _ = text

instance Pretty a => Pretty (Maybe a) where
  pp p (Just a) = pp p a
  pp _ Nothing  = empty

instance Pretty a => Pretty [a] where
  pp p as = ppList p as

instance Pretty () where
  pp _ _ = empty

instance Pretty Int8 where
  pp _ i = integer (fromIntegral i)

instance Pretty Int16 where
  pp _ i = integer (fromIntegral i)

instance Pretty Int32 where
  pp _ i = integer (fromIntegral i)

instance Pretty Int64 where
  pp _ i = integer (fromIntegral i)

instance Pretty Float where
  pp _ = float

instance Pretty Double where
  pp _ = double
