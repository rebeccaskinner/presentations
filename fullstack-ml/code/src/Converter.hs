{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Converter where
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LazyText
-- import Data.Text.Lazy.Encoding
import Text.Pandoc

import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.HTML

import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.MediaWiki
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.HTML

import Text.Pandoc.Class

import Text.Blaze.Renderer.Text

data DocumentType = DocTypeMarkdown
               | DocTypeMediaWiki
               | DocTypeLaTeX
               | DocTypeCommonMark
               | DocTypeHTML deriving (Eq)

instance Show DocumentType where
  show = \case
    DocTypeMarkdown   -> "markdown"
    DocTypeMediaWiki  -> "MediaWiki"
    DocTypeCommonMark -> "CommonMark"
    DocTypeLaTeX      -> "LaTeX"
    DocTypeHTML       -> "html"

class ShowText a where
  showText :: a -> Text

instance ShowText DocumentType where
  showText = pack . show

documentTypes :: [Text]
documentTypes =
  [ showText DocTypeMarkdown
  , showText DocTypeMediaWiki
  , showText DocTypeCommonMark
  , showText DocTypeLaTeX
  , showText DocTypeHTML
  ]

getReaderFunction :: DocumentType -> (Text -> PandocPure Pandoc)
getReaderFunction = \case
  DocTypeMarkdown   -> readMarkdown $ def { readerExtensions = githubMarkdownExtensions }
  DocTypeMediaWiki  -> readMediaWiki def
  DocTypeCommonMark -> readCommonMark def
  DocTypeLaTeX      -> readLaTeX def
  DocTypeHTML       -> readHtml def

getWriterFunction :: DocumentType -> (WriterOptions -> Pandoc -> PandocPure Text)
getWriterFunction = \case
  DocTypeMarkdown   -> writeMarkdown
  DocTypeMediaWiki  -> writeMediaWiki
  DocTypeCommonMark -> writeCommonMark
  DocTypeLaTeX      -> writeLaTeX
  DocTypeHTML       -> writeHtml5String

convertDoc :: DocumentType -> DocumentType -> String -> Either PandocError Text
convertDoc inputType outputType inputData =
  let readerF = getReaderFunction inputType
      writerF = getWriterFunction outputType
      input   = pack inputData
  in runPure $ readerF input >>= writerF def
