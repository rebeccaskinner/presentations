{-# LANGUAGE OverloadedStrings #-}
module Converter where
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
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

import Text.Blaze.Renderer.Text

data DocumentType = DocTypeMarkdown
               | DocTypeMediaWiki
               | DocTypeLaTeX
               | DocTypeCommonMark
               | DocTypeHTML deriving (Eq)

instance Show DocumentType where
  show DocTypeMarkdown = "markdown"
  show DocTypeMediaWiki = "MediaWiki"
  show DocTypeCommonMark = "CommonMark"
  show DocTypeLaTeX = "LaTeX"
  show DocTypeHTML = "html"

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

getReaderFunction :: DocumentType -> (ReaderOptions -> String -> Either PandocError Pandoc)
getReaderFunction DocTypeMarkdown = readMarkdown
getReaderFunction DocTypeMediaWiki = readMediaWiki
getReaderFunction DocTypeCommonMark = readCommonMark
getReaderFunction DocTypeLaTeX = readLaTeX
getReaderFunction DocTypeHTML = readHtml

getWriterFunction :: DocumentType -> (WriterOptions -> Pandoc -> Text)
getWriterFunction DocTypeMarkdown   = pack ... writeMarkdown
getWriterFunction DocTypeMediaWiki  = pack ... writeMediaWiki
getWriterFunction DocTypeCommonMark = pack ... writeCommonMark
getWriterFunction DocTypeLaTeX      = pack ... writeLaTeX
getWriterFunction DocTypeHTML       = renderMarkup ... writeHtml

convertDoc :: DocumentType -> DocumentType -> String -> Either PandocError Text
convertDoc inputType outputType inputData =
  let readerF = getReaderFunction inputType
      writerF = getWriterFunction outputType
  in writerF def <$> readerF def inputData

(...) = (.) . (.)
