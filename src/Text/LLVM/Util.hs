module Text.LLVM.Util where

import Control.Monad (MonadPlus,mzero)
import Data.Int (Int32)
import Data.List (intersperse,unfoldr)
import Text.PrettyPrint.HughesPJ


commas :: [Doc] -> Doc
commas  = hsep . punctuate (char ',')

colons :: [Doc] -> Doc
colons  = hcat . intersperse (char ':')

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks p = unfoldr step
  where
  step [] = Nothing
  step xs = case break p xs of
    (as,_:bs) -> Just (as,bs)
    (as,  []) -> Just (as,[])

uncons :: MonadPlus m => [a] -> m (a,[a])
uncons (a:as) = return (a,as)
uncons _      = mzero

int32 :: Int32 -> Doc
int32  = integer . fromIntegral

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

structBraces :: Doc -> Doc
structBraces body = char '{' <+> body <+> char '}'

ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe  = maybe empty

opt :: Bool -> Doc -> Doc
opt True  d = d
opt False _ = empty

