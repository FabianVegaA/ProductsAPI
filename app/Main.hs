{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Database.PostgreSQL.Simple
import Lib
import Web.Scotty (delete, get, html, param, post, put, scotty)

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "0.0.0.0",
      connectDatabase = "products",
      connectUser = "postgres",
      connectPassword = "123456"
    }

main :: IO ()
main = do
  conn <- connect localPG

  routes conn

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/" $ do
    html "Hello, world!"

  get "/hello/:name" $ do
    name <- param "name"
    html $ mconcat ["<h1>Hello ", name, "!</h1>"]

  get "/api/product/" $ getProducts conn

  get "/api/product/:id" $ getProduct conn

  post "/api/product/" $ createProduct conn

  put "/api/product/:id" $ updateProduct conn

  delete "/api/product/:id" $ deleteProduct conn
