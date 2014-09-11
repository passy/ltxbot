{-# LANGUAGE OverloadedStrings #-}

module Latex
  (renderLaTeXToFile, standaloneLaTeX)
where

import Text.LaTeX (execLaTeXT, document, raw, renderFile, LaTeXT_, LaTeXT, documentclass, ClassName, ClassOption(..))
import Data.Text (Text)

standalone :: ClassName
standalone = "standalone"

-- | Wrap a raw string to a minimal LaTeX document
standaloneLaTeX :: Monad m => Text -> LaTeXT_ m
standaloneLaTeX input = do
  documentclass [CustomOption "preview"] standalone
  document $ raw input

renderLaTeXToFile :: FilePath -> LaTeXT IO a -> IO ()
renderLaTeXToFile f tex =
  execLaTeXT tex >>= renderFile f
