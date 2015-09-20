-- |

module Yesod.Content.PDF where

import Yesod.Core.Content
import Data.ByteString
import Text.Blaze.Html
import System.Process
import System.IO.Temp
import System.IO

type PDF = ByteString

typePDF :: ContentType
typePDF = "application/pdf"

instance HasContentType PDF where
  getContentType _ = typePDF

instance ToTypedContent PDF where
    toTypedContent = TypedContent typePDF . toContent

-- pipe HTML to a temp file
-- run wkhtmltopdf reading from html tempfile and writing to tempfile
-- read from pdf tempfile and return result as IO PDF (ByteString)

url2PDF :: String -> FilePath -> Handle -> IO PDF
url2PDF url tempFile tempHandle = do
  hClose tempHandle
  (_,_,_, pHandle) <- createProcess (proc "wkhtmltopdf" ["--quiet", url, tempFile])
  waitForProcess pHandle
  Data.ByteString.readFile tempFile

html2PDF :: String -> IO ByteString
html2PDF url = withSystemTempFile "output.pdf" (url2PDF url)
