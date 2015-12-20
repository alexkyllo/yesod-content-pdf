# Yesod.Content.PDF

[![Build Status](https://travis-ci.org/alexkyllo/yesod-content-pdf.svg?branch=master)](https://travis-ci.org/alexkyllo/yesod-content-pdf)

Library for serving PDF content from a Yesod application.

The library calls the `wkhtmltopdf` command line tool to convert HTML pages to PDF documents, therefore it requires that `wkhtmltopdf` is installed and on your PATH. Tested with wkhtmltopdf versions 0.9.6 and 0.12.2.1.

Example Usage:

```haskell
-- html2PDF now takes an instance of WkhtmltopdfOptions, and a default instance is provided.
-- html2PDF :: MonadIO m => WkhtmltopdfOptions -> Html -> m PDF

-- using a dedicated handler function for serving PDF
getItemPDFR :: ItemId -> Handler PDF
getItemPDFR itemId = do
  item <- runDB $ get404 itemId
  html <- defaultLayout $(widgetFile "item")
  liftIO (html2PDF def html)

-- using provideRep to respond to requests with Accept "application/pdf"
getItemR :: ItemId -> Handler TypedContent
getItemR itemId = do
  item <- runDB $ get404 itemId
  selectRep $ do
    provideRep $ defaultLayout $(widgetFile "item") -- respond with text/html
    provideRep $ return $ toJSON item               -- respond with application/json
    provideRep $ do                                 -- respond with application/pdf
      html <- defaultLayout $(widgetFile "item")
      liftIO (html2PDF def html)
```
Available on [Hackage](https://hackage.haskell.org/package/yesod-content-pdf)

Package is usable, but in early development so the API is unstable.
Issues and Pull Requests welcome.
