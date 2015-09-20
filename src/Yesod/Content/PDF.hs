{-# OPTIONS -fno-warn-orphans #-}
-- | Utilities for serving PDF from Yesod
--   Uses and depends on command line utility wkhtmltopdf to render PDF from HTML

module Yesod.Content.PDF where
import Prelude
import Yesod.Core.Content
import Data.ByteString
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import System.Process
import System.IO.Temp
import System.IO
import System.IO.Unsafe
import Network.URI

type PDF = ByteString

typePDF :: ContentType
typePDF = "application/pdf"

instance HasContentType PDF where
  getContentType _ = typePDF

instance ToTypedContent PDF where
  toTypedContent = TypedContent typePDF . toContent

instance HasContentType (IO PDF) where
  getContentType _ = typePDF

instance ToTypedContent (IO PDF) where
  toTypedContent ioPDF = TypedContent typePDF $ toContent ioPDF

instance ToContent (IO PDF) where
  toContent = toContent . unsafePerformIO

-- | Use wkhtmltopdf to render a PDF given the URI pointing to an HTML document
uri2PDF :: URI -> IO PDF
uri2PDF uri = withSystemTempFile "output.pdf" $ uri2PDF' uri

uri2PDF' :: URI -> FilePath -> Handle -> IO PDF
uri2PDF' uri tempPDFFile tempHandle = do
  hClose tempHandle
  (_,_,_, pHandle) <- createProcess (proc "wkhtmltopdf" ["--quiet", show uri, tempPDFFile])
  _ <- waitForProcess pHandle
  Data.ByteString.readFile tempPDFFile

-- | Use wkhtmltopdf to render a PDF from an HTML (Text.Blaze.Html) type
html2PDF :: Html -> IO PDF
html2PDF html = withSystemTempFile "output.pdf" (html2PDF' html)

html2PDF' :: Html -> FilePath -> Handle -> IO PDF
html2PDF' html tempPDFFile tempPDFHandle = do
  hClose tempPDFHandle
  withSystemTempFile "input.html" $ \tempHtmlFile tempHtmlHandle -> do
    System.IO.hPutStrLn tempHtmlHandle $ renderHtml html
    hClose tempHtmlHandle
    (_,_,_, pHandle) <- createProcess (proc "wkhtmltopdf" ["--quiet", tempHtmlFile, tempPDFFile])
    _ <- waitForProcess pHandle
    Data.ByteString.readFile tempPDFFile
