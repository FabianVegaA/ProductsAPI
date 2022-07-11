# Products API

This is an example of API REST built with [Scotty](https://hackage.haskell.org/package/scotty) a web framework of Haskell and [PostgreSQL](https://www.postgresql.org) a relational database. It's a simple API to manage products.

## Index

- [Products API](#products-api)
  - [Index](#index)
  - [Quick run instructions](#quick-run-instructions)
  - [Running PostgreSQL](#running-postgresql)
  - [JSON API](#json-api)
    - [Declaring the routes of the API](#declaring-the-routes-of-the-api)
    - [The data type product](#the-data-type-product)
      - [Resume about how to use the Functors and Applicatives](#resume-about-how-to-use-the-functors-and-applicatives)
    - [Get products](#get-products)
    - [Get one product](#get-one-product)
    - [Add a product](#add-a-product)
    - [Update and Delete a product](#update-and-delete-a-product)
      - [Check if the product exists](#check-if-the-product-exists)

## Quick run instructions

> Before to start, you need to have [stack](https://docs.haskellstack.org/en/stable/README/) and [docker-compose](https://docs.docker.com/compose/) installed.

For running the API, you need to run the database using docker-compose, first is necessary to build the images and later create and start the containers with next commands:

```shell
$ docker-compose build
$ docker-compose up
```

Once start the database, install dependencies and run the API:

```shell
$ stack install
$ stack run
```

---

## Running PostgreSQL

This tutorial tries to be as simple to understand as possible, but if you know how to use docker-compose and use a PostgreSQL database, you can skip this section.

The first step is make a [`docker-compose.yml`](docker-compose.yml) file.

```yaml
version: "3"

services:
  database:
    build: ./db
    container_name: production-db
    ports:
      - "5432:5432"
    env_file:
      - example.env
    volumes:
      - ./db/postgres:/var/lib/postgresql/data
```

The [`example.env`](example.env) file is a file with the following content:

```environment
POSTGRES_USER=postgres
POSTGRES_PASSWORD=123456
POSTGRES_DB=products
```

This file is used to configure the credentials of database.

In [`docker-compose.yml`](docker-compose.yml) don't use a specific images of PostgreSQL, because it is declared in [`Dockerfile`](db/Dockerfile) this way:

```dockerfile
FROM postgres:alpine

ADD scripts/init.sql /docker-entrypoint-initdb.d

RUN chmod a+r /docker-entrypoint-initdb.d/*

EXPOSE 6666
```

Thus, the database is initialized with the file [`init.sql`](db/scripts/init.sql), only when the db is created. The file [`init.sql`](db/scripts/init.sql) is a file with the following content:

```sql
CREATE TABLE IF NOT EXISTS products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  price INTEGER NOT NULL,
  description TEXT
);

INSERT INTO products (name, price, description) VALUES
('Product 1', 10, 'Description 1'),
('Product 2', 20, 'Description 2'),
('Product 3', 30, 'Description 3');

```

This queries create the table products and insert some products.

In this way the database is available in the port 5432 and the data is stored into [db](db).

## JSON API

The first part of the API is a JSON API, with the following endpoints:

| **Method** |               **Route**               |       **Description**       |
| :--------: | :-----------------------------------: | :-------------------------: |
|    Get     |  http://localhost:8080/api/product/   |    Retrieve all products    |
|    Get     | http://localhost:8080/api/product/:id | Retrieve a specific product |
|    Post    |  http://localhost:8080/api/product/   |        Add a product        |
|    Put     | http://localhost:8080/api/product/:id |      Update a product       |
|   Delete   | http://localhost:8080/api/product/:id |      Delete a product       |

### Declaring the routes of the API

The routes are declaring in the file [`Main.hs`](Main.hs), but before, as it is common in Haskell, it is necessary to build a `main` function where the application is executing. Here where the connection to the database is also creating:

```haskell
import Database.PostgreSQL.Simple

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
  {- continue -}
```

Then, for the routes, you can create a function with the routes to separate these processes.

```haskell
import Lib
import Web.Scotty (delete, get, post, put, scotty)

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/api/product/" $ getProducts conn

  get "/api/product/:id" $ getProduct conn

  post "/api/product/" $ createProduct conn

  put "/api/product/:id" $ updateProduct conn

  delete "/api/product/:id" $ deleteProduct conn
```

This is called from `main`:

```haskell
main :: IO ()
main = do
  conn <- connect localPG

  routes conn
```

### The data type product

Before creating the functions to get the products and use it, you must create the data type to product.

```haskell
data Product = Product
  { idProduct :: Int,
    name :: String,
    price :: Int,
    description :: String
  }
```

This allows to receive the data from the database and manage it in a better way, but defining only the data type is not enough for Haskell to parse the data from the database to the `Product` type. To do this, we need to **instance** from the `FromRow` type class the functions and define how the data is translated to the type we define.

> Yes, Haskell also has a class, but it is not a class as in the OOP paradigm. It can be considered as a kind of types. For more information see the following explanation from [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101).

```haskell
import Database.PostgreSQL.Simple.FromRow

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> field <*> field
```

> The operators `<*>` and `<$>` are explained in [here](#resume-of-how-to-use-the-functors-and-applicatives)

Now it only remains to do the same to be able to serialize the type `Product` to a JSON and vice versa. That is possible instancing from the type class FromJSON and ToJSON, in this way:

```haskell
import Data.Aeson

instance FromJSON Product where
  parseJSON (Object o) =
    Product <$> o .:? "id" .!= 0
      <*> o .: "name"
      <*> o .: "price"
      <*> o .: "description"
  parseJSON _ = fail "Expected an object for Product"
```

This mean that now we have defined the function `parseJSON`, this receives a JSON and when this JSON is an object, it will look for the fields `id`, `name`, `price` and `description`. For that, we use the `.:` and `.:?` operators, which means get field and maybe get field, correspondingly. And the operator `.!=` is used to set the default value for the field if it is not found when we use `.:?`.

---

#### Resume about how to use the Functors and Applicatives

> Haskell take advantage of the property of the Functors and Applicatives type classes.
> <br/><br/>The Functor class has a function called `fmap` that allows to map a function over a functor, also can be used with the operator `<$>`.
> <br/><br/>The Applicatives is a subclass of the Functor class, and has a function called `<*>` that allows to apply a function wraped into a functor to a other functor.
> <br/><br/>So, when we use `<$>` and `<*>` means, take the constructor of type `Product` and apply it to the result to get a field. We still need more field to have a `Product` type, so now it is a function wrapped by a functor. To apply the wrapped function, we use `<*>` and so on until we construct the `Product` type with all its fields.

---

And similarly to convert a `Product` type to a JSON:

```haskell
import Data.Aeson

instance ToJSON Product where
  toJSON (Product idProduct name price description) =
    object
      [ "id" .= idProduct,
        "name" .= name,
        "price" .= price,
        "description" .= description
      ]
```

> The `object` function is used to create a JSON object, and the `.=` assigns a value to a field in the object.

### Get products

The function `getProducts` is a function that response a list of products.

> Why have I said that the function **response** a list of products and not **return** a list of products?
> That's because the function truly return a `ActionM ()`, this is similar to `IO` monad and it is way to produce side effects.

```haskell
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Web.Scotty (ActionM)
import qualified Web.Scotty as S

getProducts :: Connection -> ActionM ()
getProducts conn = do
  products <- (liftIO $ query_ conn "SELECT * FROM products") :: ActionM [Product]
  S.json $ object ["products" .= products]
```

| line 6 | This function receives a connection to the database and returns an empty `ActionM`, this is very similar to a `IO` monad, but in simple words, it provides a response to the client. |
| :----: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

> This is the way that Haskell control the [**side effects**](<https://en.wikipedia.org/wiki/Side_effect_(computer_science)>) for maintain the [**referential transparency**](https://en.wikipedia.org/wiki/Referential_transparency) since Haskell is a [**pure functional language**](https://wiki.haskell.org/Pure).

| line 7 | Save connection into `conn`. |
| :----: | :--------------------------- |

| line 8 | This executes the query to get all the saved products. <br/>The `query_` function is very similar to `query` the difference is that this one does not receive values to replace in the query. <br/>Also, the result of this query is interpreting as an `IO [Product]` type and `liftIO` is a function that take a `IO a` and transform to `m a` value, where `m` is another Monad. That is, it transforms `IO [Product]` result of `query_` into `ActionM [Product]`, that is why the `::` operator is using here, to give information to Haskell about final type. |
| :----: | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

| line 9 | This return a `json` with an **object** that into have `"products"` with a list of products. |
| :----: | :------------------------------------------------------------------------------------------- |

So, if you query the route `http://localhost:8080/api/product/` you get something like this:

```json
{
  "products": [
    {
      "description": "Description 1",
      "id": 1,
      "name": "Product 1",
      "price": 10
    },
    {
      "description": "Description 2",
      "id": 2,
      "name": "Product 2",
      "price": 20
    },
    {
      "description": "Description 3",
      "id": 3,
      "name": "Product 3",
      "price": 30
    }
  ]
}
```

### Get one product

The function `getProduct` is a function that retrieve a id product and response a product.

```haskell
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status (status200, status400)
import Web.Scotty (ActionM, param, status)

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
```

| line 2 | This function is similar to the `getProducts` function, but it receives the identifier of some product, for this the `param` function is using, and the result is setting as an `ActionM Int` type. |
| :----: | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

| line 3 | Here we use the `query` function, it can format any query using values wrapped with `Only`. Thus, we get a custom query by adding `'?'` where a field should be added. |
| :----: | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

| line 5 - line 11 | Depending on the result of the query, it is possible to obtain an empty list or a list with at least one value, and the best way to declare this is with [pattern matching](https://en.wikipedia.org/wiki/Pattern_matching). If the list is empty the result is a status code 400 and a message of error, else if the list have at least one value return a status code 200 and the first value of the list. |
| :--------------: | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

### Add a product

The function `createProduct` is very similar to the previous functions. A different thing is that here is used the `execute` function and not `query`, which is used to execute a query and return the number of rows affected. This result is used to determine if the product was created or not. Other different is that now we use the `jsonData` function to get the JSON data from the request, for this work we created the `intances` of the `FromJSON` typeclass.

```haskell
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status (status201, status400)
import Web.Scotty (ActionM, param, status)


createProduct :: Connection -> ActionM ()
createProduct conn = do
  (Product _ _name _price _description) <- jsonData
  let result =
        execute
          conn
          "INSERT INTO products (name, price, description) VALUES (?, ?, ?)"
          (_name, _price, _description)
  n <- liftIO result
  if n > 0
    then do
      status status201
      S.json $ object ["message" .= ("Product created" :: String)]
    else do
      status status400
      S.json $ object ["error" .= ("Product not created" :: String)]
```

### Update and Delete a product

And so on, the function `updateProduct` and `deleteProduct` are similar to the previous functions, but for do more easy to know if the product exists or not, we going to create the function [`hasProduct`](#check-if-the-product-exists) to check if the product exists.

```haskell
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status (status200, status204)
import Web.Scotty (ActionM, jsonData, param, status)

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
```

```haskell
deleteProduct :: Connection -> ActionM ()
deleteProduct conn = do
  _idProduct <- param "id" :: ActionM Int

  productExist <- liftIO $ haveProduct conn _idProduct

  if not productExist
    then do
      status status400
      S.json $ object ["error" .= ("Product not found" :: String)]
    else do
      status status200
      liftIO $ execute conn "DELETE FROM products WHERE id = ?" (Only _idProduct)
      S.json $ object ["message" .= ("Product deleted" :: String)]

```

#### Check if the product exists

```haskell
import Database.PostgreSQL.Simple

haveProduct :: Connection -> Int -> IO Bool
haveProduct conn _idProduct = do
  [Only n] <- query conn "SELECT COUNT(*) FROM products WHERE id = ?" (Only _idProduct) :: IO [Only Int]
  return $ n > 0

```
