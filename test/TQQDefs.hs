{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TQQDefs where

import Data.String
import Language.Haskell.TH ( ExpQ )
import Language.Haskell.TH.Quote
import Numeric.Natural


sq :: QuasiQuoter
sq = QuasiQuoter extractor
     (error "no q patterns")
     (error "no q types")
     (error "no q dec")

extractor :: String -> ExpQ
extractor s =
  case inSeps $ filter (/= '\r') s of
    Post a -> [|fromString a|]
    Pre _ -> error $ "No starting line found"
    MatchLine _ -> error $ "Only starting line found"
    Pass _ _ _ -> error $ "No ending line found"


data QState = Pre String
            | MatchLine Natural
            | Pass Natural Natural String
            | Post String

inSeps :: String -> QState
inSeps =
  let sep = "----"
      sepl = length sep
      matchSep s = if take sepl s == reverse sep
                   then let ss n = \case
                              [] -> Nothing
                              ('\n':_) -> Just n
                              (' ':cs) -> ss (n+1) cs
                              _ -> Nothing
                        in ss 0 $ drop sepl s
                   else Nothing
      nxtC :: QState -> Char -> QState
      nxtC = \case
        (Post s) -> const $ Post s
        (Pre p) -> \c -> let p' = c : p
                         in case matchSep p' of
                              Just n -> MatchLine n
                              Nothing -> Pre p'
        (MatchLine n) -> \c -> if '\n' == c then Pass n n "" else MatchLine n
        (Pass n p s) -> \c ->
          let s' = c : s
          in if p == 0
             then if c == '\n' && n > 0
                  then Pass n n s'
                  else if take sepl s' == reverse sep
                       then Post $ reverse $ drop (sepl + 1) s'
                       else Pass n 0 s'
             else Pass n (p-1) s
    in foldl nxtC (Pre "")
