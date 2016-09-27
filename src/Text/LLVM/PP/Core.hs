module Text.LLVM.PP.Core where

import           Data.Int (Int32)
import qualified Text.PrettyPrint.HughesPJ as PP


-- Pretty-printer Core ---------------------------------------------------------

-- | The differences between various versions of the llvm textual AST.
data Config = Config { cfgLoadImplicitType :: Bool
                       -- ^ True when the type of the result of a load is
                       -- derived from its pointer argument, or supplied
                       -- implicitly.
                     }


ppLLVM, ppLLVM35, ppLLVM36, ppLLVM37 :: Doc -> PP.Doc

ppLLVM = ppLLVM37

ppLLVM35 = ppLLVM36

ppLLVM36 = runDoc Config { cfgLoadImplicitType = True
                         }

ppLLVM37 = runDoc Config { cfgLoadImplicitType = False
                         }

type Doc = DocM PP.Doc

newtype DocM a = DocM { unDoc :: Config -> a }

instance Functor DocM where
  fmap f (DocM g) = DocM (f . g)

instance Applicative DocM where
  pure x            = DocM (const x)
  DocM f <*> DocM x = DocM (\cfg -> f cfg (x cfg))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad DocM where
  return       = pure
  DocM m >>= f = DocM (\cfg -> unDoc (f (m cfg)) cfg)
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

runDoc :: Config -> Doc -> PP.Doc
runDoc  = flip unDoc

binop :: (PP.Doc -> PP.Doc -> PP.Doc) -> (Doc -> Doc -> Doc)
binop f = \ (DocM l) (DocM r) -> DocM (\ cfg -> f (l cfg) (r cfg))
{-# INLINE binop #-}

unary :: (PP.Doc -> PP.Doc) -> (Doc -> Doc)
unary f = \ (DocM p) -> DocM (\cfg -> f (p cfg))
{-# INLINE unary #-}

lift :: PP.Doc -> Doc
lift val = DocM (const val)
{-# INLINE lift #-}

checkConfig :: (Config -> a) -> DocM a
checkConfig  = DocM


-- Binary Operators ------------------------------------------------------------

(<>), (<+>), ($$), ($+$) :: Doc -> Doc -> Doc

(<>)  = binop (PP.<>)
(<+>) = binop (PP.<+>)
($$)  = binop (PP.$$)
($+$) = binop (PP.$+$)


-- Unary Operators -------------------------------------------------------------

parens, doubleQuotes, braces, brackets :: Doc -> Doc

brackets     = unary PP.brackets
braces       = unary PP.braces
parens       = unary PP.parens
doubleQuotes = unary PP.doubleQuotes


-- List Operators --------------------------------------------------------------

sep :: [Doc] -> Doc
sep ds = DocM (\cfg -> PP.sep (map (runDoc cfg) ds))

hcat :: [Doc] -> Doc
hcat ds = DocM (\cfg -> PP.hcat (map (runDoc cfg) ds))

vcat :: [Doc] -> Doc
vcat ds = DocM (\cfg -> PP.vcat (map (runDoc cfg) ds))

commas :: [Doc] -> Doc
commas ds = DocM (\cfg -> PP.fsep (PP.punctuate PP.comma (map (runDoc cfg) ds)))

colons :: [Doc] -> Doc
colons ds = DocM (\cfg -> PP.fsep (PP.punctuate (PP.char ':') (map (runDoc cfg) ds)))


-- Combinators -----------------------------------------------------------------

nest :: Int -> Doc -> Doc
nest i d = DocM (\cfg -> PP.nest i (runDoc cfg d))

empty :: Doc
empty  = lift PP.empty

comma :: Doc
comma  = lift PP.comma

char :: Char -> Doc
char  = lift . PP.char

text :: String -> Doc
text  = lift . PP.text

int :: Int -> Doc
int  = lift . PP.int

integer :: Integer -> Doc
integer  = lift . PP.integer

int32 :: Int32 -> Doc
int32  = integer . fromIntegral

float :: Float -> Doc
float  = lift . PP.float

double :: Double -> Doc
double  = lift . PP.double

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

structBraces :: Doc -> Doc
structBraces body = char '{' <+> body <+> char '}'

ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe  = maybe empty

opt :: Bool -> Doc -> Doc
opt True  d = d
opt False _ = empty
