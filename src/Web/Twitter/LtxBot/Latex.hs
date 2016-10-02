{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.LtxBot.Latex
  (renderLaTeXStatus)
where

import Text.LaTeX (document, raw, documentclass, ClassName, ClassOption(..), render, LaTeX)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Web.Twitter.Types (Status)
import qualified Web.Twitter.Types.Lens as TL
import Control.Lens ((^.))

standalone :: ClassName
standalone = "standalone"

-- | Wrap a raw string to a minimal LaTeX document
standaloneLaTeX :: T.Text -> LaTeX
standaloneLaTeX input =
     documentclass [CustomOption "preview"] standalone
  <> document (raw input)

wrapLaTeXStatus :: Status -> LaTeX
wrapLaTeXStatus s = standaloneLaTeX (s ^. TL.text)

renderLaTeXStatus :: Status -> T.Text
renderLaTeXStatus s = render (wrapLaTeXStatus s)
