{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getProduct,
    getProducts,
    createProduct,
    updateProduct,
    addProduct,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Web.Scotty (ActionM, param, post, status, text)
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

getProducts :: Connection -> ActionM ()
getProducts conn = do
  products <- (liftIO $ query_ conn "SELECT * FROM products") :: ActionM [Product]
  S.json $ object ["products" .= products]

getProduct :: Connection -> ActionM ()
getProduct conn = do
  _idProduct <- param "id" :: ActionM Int
  let result = query conn "SELECT * FROM products WHERE id = ?" (Only _idProduct)
  product <- liftIO result :: ActionM [Product]
  S.json (head product)

addProduct :: Connection -> ActionM ()
addProduct conn = do
  _nameProduct <- param "name" :: ActionM String

  hasProductResult <- liftIO $ hasProduct conn _nameProduct
  if hasProductResult
    then updateProduct conn
    else createProduct conn

hasProduct :: Connection -> String -> IO Bool
hasProduct conn _nameProduct = do
  n <- execute conn "SELECT * FROM products WHERE name = ?" (Only _nameProduct)
  return $ n > 0

createProduct :: Connection -> ActionM ()
createProduct conn = do
  _name <- param "name" :: ActionM String
  _price <- param "price" :: ActionM Int
  _description <- param "description" :: ActionM String
  let result =
        execute
          conn
          "INSERT INTO products (name, price, description) VALUES (?, ?, ?)"
          (_name, _price, _description)
  liftIO result

  S.json $ object ["message" .= ("Product created successfully!" :: String)]

updateProduct :: Connection -> ActionM ()
updateProduct conn = do
  _name <- param "name" :: ActionM String
  _price <- param "price" :: ActionM Int
  _description <- param "description" :: ActionM String
  let result =
        execute
          conn
          "UPDATE products SET name = ?, price = ?, description = ? WHERE name = ?"
          (_name, _price, _description, _name)
  liftIO result
  S.json $ object ["message" .= ("Product updated" :: String)]
