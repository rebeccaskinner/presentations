{-# LANGUAGE OverloadedStrings #-}
module Viewer (runApp, app) where

import Data.Aeson (Value(..), object, (.=))
import Network.Wai (Application)
import Network
import qualified Web.Scotty as S
import Data.Text.Lazy as Text
import Converter
import Network.HTTP.Types.Status
import Control.Monad (unless)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B

app :: IO Application
app = S.scottyApp app'

runApp :: Int -> IO ()
runApp p = S.scotty (fromIntegral p) app'

debugMsg :: DocumentType -> DocumentType -> String -> IO ()
debugMsg outType inType body = do
  putStrLn $ "input type:  " ++ show inType
  putStrLn $ "output type: " ++ show outType
  putStrLn "body:"
  putStrLn body
  putStrLn "--------------"

app' :: S.ScottyM ()
app' = do
  S.get "/" $ S.file "frontend/index.html"
  S.get "/supportedformats" $ S.json documentTypes
  S.get "/output-formats" $ S.json documentTypes

  S.post "/:render" $ do
    outputFormat <- S.param "render" :: S.ActionM DocumentType
    inputFormat  <- extractParam "format"
    body <- B.unpack <$> S.body
    liftIO $ debugMsg outputFormat inputFormat body
    case convertDoc inputFormat outputFormat body of
      Left errMsg -> do
        S.status status500
        (S.text . pack . show) errMsg
        S.finish
      Right result -> S.text result

extractParam :: Text -> S.ActionM DocumentType
extractParam paramName = do
  rawText <- S.param paramName
  case S.parseParam rawText of
    Left errMsg -> do
      S.status status404
      S.text errMsg
      S.finish
    Right parsed -> return parsed

-- Make Converter.DocTypeType an instance of Parsable so that we can
-- read it in from the query param directly
instance S.Parsable DocumentType where
  parseParam msg =
    case toLower msg of
      "markdown"   -> Right DocTypeMarkdown
      "mediawiki"  -> Right DocTypeMediaWiki
      "commonmark" -> Right DocTypeCommonMark
      "latex"      -> Right DocTypeLaTeX
      "html"       -> Right DocTypeHTML
      _            -> Left "Uknown input format"
