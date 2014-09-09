{-# LANGUAGE OverloadedStrings #-}

module Latex
  (renderLaTeXToFile)
where

import Text.LaTeX (execLaTeXT, document, raw, renderFile)
import Data.Text (Text)

renderLaTeXToFile :: Text -> FilePath -> IO ()
renderLaTeXToFile input f =
  execLaTeXT (document $ raw input) >>= renderFile f
