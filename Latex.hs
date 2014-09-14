{-# LANGUAGE OverloadedStrings #-}

module Latex
  (renderLaTeXToHandle, standaloneLaTeX)
where

import qualified Data.ByteString as B
import Text.LaTeX (execLaTeXT, document, raw, LaTeXT_, LaTeXT, documentclass, ClassName, ClassOption(..), Render, render)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.IO (Handle)

standalone :: ClassName
standalone = "standalone"

-- | Wrap a raw string to a minimal LaTeX document
standaloneLaTeX :: Monad m => Text -> LaTeXT_ m
standaloneLaTeX input = do
  documentclass [CustomOption "preview"] standalone
  document $ raw input

-- | Use this function to render a 'LaTeX' (or another
--   one in the 'Render' class) value directly
--   into a file handle.
renderHandle :: Render a => Handle -> a -> IO ()
renderHandle f = B.hPutStr f . encodeUtf8 . render

renderLaTeXToHandle :: Handle -> LaTeXT IO a -> IO ()
renderLaTeXToHandle f tex =
  execLaTeXT tex >>= renderHandle f
