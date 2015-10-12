-- | Utilities for serving PDF from Yesod.
--   Uses and depends on command line utility wkhtmltopdf to render PDF from HTML.
module Yesod.Content.PDF
  ( -- * Conversion
    uri2PDF
  , html2PDF
    -- * Data type
  , PDF(..)
  , typePDF
  ) where

import Blaze.ByteString.Builder.ByteString
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString
import Data.Conduit
import Network.URI
import System.IO
import System.IO.Temp
import System.Process
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import Yesod.Core.Content

newtype PDF = PDF ByteString

-- | Provide MIME type "application/pdf" as a ContentType for Yesod.
typePDF :: ContentType
typePDF = "application/pdf"

instance HasContentType PDF where
  getContentType _ = typePDF

instance ToTypedContent PDF where
  toTypedContent = TypedContent typePDF . toContent

instance ToContent PDF where
  toContent (PDF bs) = ContentSource $ do
    yield $ Chunk $ fromByteString bs

-- | Use wkhtmltopdf to render a PDF given the URI pointing to an HTML document.
uri2PDF :: MonadIO m => URI -> m PDF
uri2PDF = wkhtmltopdf . flip ($) . show

-- | Use wkhtmltopdf to render a PDF from an HTML (Text.Blaze.Html) type.
html2PDF :: MonadIO m => Html -> m PDF
html2PDF html =
  wkhtmltopdf $ \inner ->
  withSystemTempFile "input.html" $ \tempHtmlFp tempHtmlHandle -> do
    System.IO.hPutStrLn tempHtmlHandle $ renderHtml html
    hClose tempHtmlHandle
    inner tempHtmlFp

-- | (Internal) Call wkhtmltopdf.
wkhtmltopdf :: MonadIO m => ((String -> IO PDF) -> IO PDF) -> m PDF
wkhtmltopdf setupInput =
  liftIO $
  withSystemTempFile "output.pdf" $ \tempOutputFp tempOutputHandle -> do
    hClose tempOutputHandle
    setupInput $ \inputArg -> do
      (_, _, _, pHandle) <- createProcess (proc "wkhtmltopdf" ["--quiet", inputArg, tempOutputFp])
      _ <- waitForProcess pHandle
      PDF <$> Data.ByteString.readFile tempOutputFp
