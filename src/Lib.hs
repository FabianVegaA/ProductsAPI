{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getProduct,
    getProducts,
    createProduct,
    updateProduct,
    deleteProduct,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.HTTP.Types.Status (Status, status200, status201, status204, status400)
import Web.Scotty (ActionM, jsonData, param, post, status, text)
import qualified Web.Scotty as S

data Product = Product
  { idProduct :: Int,
    name :: String,
    price :: Int,
    description :: String
  }
  deriving (Show)

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> field <*> field

instance ToJSON Product where
  toJSON (Product idProduct name price description) =
    object
      [ "id" .= idProduct,
        "name" .= name,
        "price" .= price,
        "description" .= description
      ]

instance FromJSON Product where
  parseJSON (Object o) =
    Product <$> o .:? "id" .!= 0
      <*> o .: "name"
      <*> o .: "price"
      <*> o .: "description"
  parseJSON _ = fail "Expected an object for Product"

getProducts :: Connection -> ActionM ()
getProducts conn = do
  products <- (liftIO $ query_ conn "SELECT * FROM products") :: ActionM [Product]
  status status200
  S.json $ object ["products" .= products]
 
getProduct :: Connection -> ActionM ()
getProduct conn = do
  _idProduct <- param "id" :: ActionM Int
  let result = query conn "SELECT * FROM products WHERE id = ?" (Only _idProduct)
  product <- liftIO result :: ActionM [Product]
  case product of
    [] -> do
      status status400
      S.json $ object ["error" .= ("Product not found" :: String)]
    _ -> do
      status status200
      S.json (head product)

createProduct :: Connection -> ActionM ()
createProduct conn = do
  (Product _ _name _price _description) <- jsonData
  let result =
        execute
          conn
          "INSERT INTO products (name, price, description) VALUES (?, ?, ?)"
          (_name, _price, _description)
  n <- liftIO result
  status $ if n > 0 then status201 else status400
  S.json $ object ["message" .= ("Product created successfully!" :: String)]

updateProduct :: Connection -> ActionM ()
updateProduct conn = do
  _idProduct <- param "id" :: ActionM Int

  (Product _ _name _price _description) <- jsonData :: ActionM Product

  productExist <- liftIO $ haveProduct conn _idProduct

  if not productExist
    then createProduct conn
    else do
      let result =
            execute
              conn
              "UPDATE products SET name = ?, price = ?, description = ? WHERE id = ?"
              (_name, _price, _description, _idProduct)
      n <- liftIO result
      status $ if n > 0 then status200 else status204
      S.json $ object ["message" .= ("Product updated" :: String)]

deleteProduct :: Connection -> ActionM ()
deleteProduct conn = do
  _idProduct <- param "id" :: ActionM Int
  let result = execute conn "DELETE FROM products WHERE id = ?" (Only _idProduct)
  n <- liftIO result
  status $ if n > 0 then status200 else status400
  S.json $ object ["message" .= ("Product deleted" :: String)]

haveProduct :: Connection -> Int -> IO Bool
haveProduct conn _idProduct = do
  [Only n] <- query conn "SELECT COUNT(*) FROM products WHERE id = ?" (Only _idProduct) :: IO [Only Int]
  return $ n > 0
