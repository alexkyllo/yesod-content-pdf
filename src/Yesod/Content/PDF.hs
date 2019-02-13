-- | Utilities for serving PDF from Yesod.
--   Uses and depends on command line utility wkhtmltopdf to render PDF from HTML.
module Yesod.Content.PDF
  ( -- * Conversion
    uri2PDF
  , html2PDF
    -- * Data type
  , PDF(..)
  , typePDF
    -- * Options
  , def
  , WkhtmltopdfOptions
  , wkCollate
  , wkCopies
  , wkGrayscale
  , wkLowQuality
  , wkPageSize
  , wkOrientation
  , wkDisableSmartShrinking
  , wkTitle
  , wkMarginBottom
  , wkMarginLeft
  , wkMarginRight
  , wkMarginTop
  , wkZoom
  , wkJavascriptDelay
  , wkWindowStatus
  , PageSize(..)
  , Orientation(..)
  , UnitReal(..)
  ) where

import Prelude
import Blaze.ByteString.Builder.ByteString
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString
import Data.ByteString.Builder (hPutBuilder)
import Data.Conduit
import Data.Default (Default(..))
import Network.URI
import System.IO
import System.IO.Temp
import System.Process
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8
import Yesod.Core.Content

newtype PDF = PDF { pdfBytes :: ByteString }
            deriving (Eq, Ord, Read, Show)

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
uri2PDF :: MonadIO m => WkhtmltopdfOptions -> URI -> m PDF
uri2PDF opts = wkhtmltopdf opts . flip ($) . show

-- | Use wkhtmltopdf to render a PDF from an HTML (Text.Blaze.Html) type.
html2PDF :: MonadIO m => WkhtmltopdfOptions -> Html -> m PDF
html2PDF opts html =
  wkhtmltopdf opts $ \inner ->
  withSystemTempFile "input.html" $ \tempHtmlFp tempHtmlHandle -> do
    hSetBinaryMode tempHtmlHandle True
    hSetBuffering  tempHtmlHandle $ BlockBuffering Nothing
    hPutBuilder    tempHtmlHandle $ renderHtmlBuilder html
    hClose         tempHtmlHandle
    inner tempHtmlFp

-- | (Internal) Call wkhtmltopdf.
wkhtmltopdf :: MonadIO m => WkhtmltopdfOptions -> ((String -> IO PDF) -> IO PDF) -> m PDF
wkhtmltopdf opts setupInput =
  liftIO $
  withSystemTempFile "output.pdf" $ \tempOutputFp tempOutputHandle -> do
    hClose tempOutputHandle
    setupInput $ \inputArg -> do
      let args = toArgs opts ++ [inputArg, tempOutputFp]
      (_, _, _, pHandle) <- createProcess (proc "wkhtmltopdf" args)
      _ <- waitForProcess pHandle
      PDF <$> Data.ByteString.readFile tempOutputFp

-- | Options passed to wkhtmltopdf.  Please use the 'def' value
-- and then modify individual settings. For more information, see
-- <http://www.yesodweb.com/book/settings-types>.
data WkhtmltopdfOptions =
  WkhtmltopdfOptions
    { wkCollate         :: Bool
      -- ^ Collate when printing multiple copies.
    , wkCopies          :: Int
      -- ^ Number of copies to print into the PDF file.
    , wkGrayscale       :: Bool
      -- ^ Whether output PDF should be in grayscale.
    , wkLowQuality      :: Bool
      -- ^ Generate lower quality output to conserve space.
    , wkPageSize        :: PageSize
      -- ^ Page size (e.g. "A4", "Letter").
    , wkOrientation     :: Orientation
      -- ^ Orientation of the output.
    , wkDisableSmartShrinking  :: Bool
      -- ^ Intelligent shrinking strategy used by WebKit that makes the pixel/dpi ratio none constant.
    , wkTitle           :: Maybe String
      -- ^ Title of the generated PDF file.
    , wkMarginBottom    :: UnitReal
      -- ^ Bottom margin size.
    , wkMarginLeft      :: UnitReal
      -- ^ Left margin size.
    , wkMarginRight     :: UnitReal
      -- ^ Right margin size.
    , wkMarginTop       :: UnitReal
      -- ^ Top margin size.
    , wkZoom            :: Double
      -- ^ Zoom factor.
    , wkJavascriptDelay :: Maybe Int
      -- ^ Time to wait for Javascript to finish in milliseconds.
    , wkWindowStatus    :: Maybe String
      -- ^ String to wait for window.status to be set to.
    } deriving (Eq, Ord, Show)

instance Default WkhtmltopdfOptions where
  def = WkhtmltopdfOptions
    { wkCollate               = True
    , wkCopies                = 1
    , wkGrayscale             = False
    , wkLowQuality            = False
    , wkPageSize              = A4
    , wkOrientation           = Portrait
    , wkDisableSmartShrinking = False
    , wkTitle                 = Nothing
    , wkMarginBottom          = Mm 10
    , wkMarginLeft            = Mm 0
    , wkMarginRight           = Mm 0
    , wkMarginTop             = Mm 10
    , wkZoom                  = 1
    , wkJavascriptDelay       = Nothing
    , wkWindowStatus          = Nothing
    }

-- | Cf. 'wkPageSize'.
data PageSize =
    A4
  | Letter
  | OtherPageSize String -- ^ <http://doc.qt.io/qt-4.8/qprinter.html#PaperSize-enum>.
  | CustomPageSize UnitReal UnitReal -- ^ Height and width.
  deriving (Eq, Ord, Show)

-- | Cf. 'wkOrientation'.
data Orientation =
    Portrait
  | Landscape
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A unit of measure.
data UnitReal =
    Mm Double
  | Cm Double
  | OtherUnitReal String
  deriving (Eq, Ord, Show)

-- | (Internal) Convert options to arguments.
class ToArgs a where
  toArgs :: a -> [String]

instance ToArgs WkhtmltopdfOptions where
  toArgs opts =
      [ "--quiet"
      , if wkCollate opts then "--collate" else "--no-collate"
      , "--copies", show (wkCopies opts)
      , "--zoom",   show (wkZoom   opts)
      ] ++
      Prelude.concat
       [ [ "--grayscale"  | True <- [wkGrayscale  opts] ]
       , [ "--lowquality" | True <- [wkLowQuality opts] ]
       , [ "--disable-smart-shrinking" | True <- [wkDisableSmartShrinking opts] ]
       , toArgs (wkPageSize    opts)
       , toArgs (wkOrientation opts)
       , maybe [] (\t -> ["--title",            t     ]) (wkTitle           opts)
       , maybe [] (\d -> ["--javascript-delay", show d]) (wkJavascriptDelay opts)
       , maybe [] (\s -> ["--window-status",    s     ]) (wkWindowStatus    opts)
       , "--margin-bottom" : toArgs (wkMarginBottom opts)
       , "--margin-left"   : toArgs (wkMarginLeft   opts)
       , "--margin-right"  : toArgs (wkMarginRight  opts)
       , "--margin-top"    : toArgs (wkMarginTop    opts)
       ]

instance ToArgs PageSize where
  toArgs A4                   = ["--page-size", "A4"]
  toArgs Letter               = ["--page-size", "Letter"]
  toArgs (OtherPageSize s)    = ["--page-size", s]
  toArgs (CustomPageSize h w) = ("--page-height" : toArgs h) ++ ("--page-width" : toArgs w)

instance ToArgs Orientation where
  toArgs o = ["--orientation", show o]

instance ToArgs UnitReal where
  toArgs (Mm x)            = [show x ++ "mm"]
  toArgs (Cm x)            = [show x ++ "cm"]
  toArgs (OtherUnitReal s) = [s]
