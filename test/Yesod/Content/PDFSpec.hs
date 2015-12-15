-- |

module Yesod.Content.PDFSpec (main, spec) where

import Test.Hspec
import Yesod.Content.PDF
import Data.ByteString.UTF8 (toString)
import Text.Blaze.Html

main :: IO ()
main = hspec spec

h :: Html
h = toHtml ("<html><body>Hello World</body></html>" :: String)

spec :: Spec
spec = do
  describe "generate PDF from HTML document" $ do
    it "should generate PDF from html document" $ do
      (fmap ((take 8) . toString . pdfBytes) (html2PDF def h)) `shouldReturn` ("%PDF-1.4" :: String)
