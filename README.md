* Yesod.Content.PDF

Library for serving PDF content from a Yesod application.
Uses `wkhtmltopdf` command line tool to convert HTML pages to PDF documents.
Requires that `wkhtmltopdf` is installed and on your PATH.

Example Usage:

```haskell

-- using a dedicated handler function for serving PDF
getItemPDFR :: ItemId -> Handler PDF
getItemPDFR itemId = do
  item <- runDB $ get404 itemId
  html <- defaultLayout $(widgetFile "item")
  liftIO (html2PDF html)

-- using provideRep to respond to requests with Accept "application/pdf"
getItemR :: ItemId -> Handler TypedContent
getItemR itemId = do
  item <- runDB $ get404 itemId
  selectRep $ do
    provideRep $ defaultLayout $(widgetFile "item") -- respond with text/html
    provideRep $ return $ toJSON item               -- respond with application/json
    provideRep $ do                                 -- respond with application/pdf
      html <- defaultLayout $(widgetFile "item")
      liftIO (html2PDF html)
```
