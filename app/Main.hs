{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Web.Scotty (get, html, param, post, scotty)

main :: IO ()
main = scotty 8080 $ do
  get "/" $ do
    html "Hello, world!"

  get "/hello/:name" $ do
    name <- param "name"
    html $ mconcat ["<h1>Hello ", name, "!</h1>"]

  get "/api/product/" $ do
    html "Get products"

  get "/api/product/:id" $ do
    idProduct <- param "id"
    html $ mconcat ["Get product with id: ", idProduct]

  post "/api/product/" $ do
    html "Update product and Add new product"
