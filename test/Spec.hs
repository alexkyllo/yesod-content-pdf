import Test.Hspec
import qualified Yesod.Content.PDFSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Yesod.Content.PDF" Yesod.Content.PDFSpec.spec
