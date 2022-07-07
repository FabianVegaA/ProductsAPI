# Products API

This is an example of API REST built with [Scotty]() a web framework of Haskell and [PostgreSQL]() a relational database. It's a simple API to manage products.

## Index

- [Products API](#products-api)
  - [Index](#index)
  - [Quick run instructions](#quick-run-instructions)
  - [Running PostgreSQL](#running-postgresql)
  - [JSON API](#json-api)
    - [Declaring the routes of the API](#declaring-the-routes-of-the-api)
    - [Get products](#get-products)
  - [HTML API](#html-api)

## Quick run instructions

> Before to start, you need have installed [stack]() and [docker-compose]() installed.

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

The tutorial tries to be as simple to understand as possible, but if you know how to use docker-compose and use a PostgreSQL database, you can skip this section.

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

The queries create the table products and insert some products.

In this way the database is available in the port 5432 and the data is stored into [db](db).

## JSON API

The first part of the API is a JSON API, with the following endpoints:

| **Method** |               **Route**               |       **Description**       |
| :--------: | :-----------------------------------: | :-------------------------: |
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

Later, for the routes I create a function with the routes:

```haskell
import Lib
import Web.Scotty (get, html, param, post, put, scotty)

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

### Get products

The function `getProducts` is a function that returns a list of products.

```haskell
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Database.PostgreSQL.Simple
import Web.Scotty (ActionM)
import qualified Web.Scotty as S

getProducts :: Connection -> ActionM ()
getProducts conn = do
  products <- (liftIO $ query_ conn "SELECT * FROM products") :: ActionM [Product]
  S.json $ object ["products" .= products]
```

| line 0 | This function receives a connection to the database and returns an empty `ActionM`, this is very similar to a `IO` monad, but in simple words, it provides a response to the client. |
| :----: | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |

> This is the way that Haskell control the [**side effects**]() for maintain the [**referential transparency**]() since Haskell is a [**pure functional language**]().

| line 1 | Save connection into `conn` |
| :----: | :--------------------------- |

| line 2 | This executes the query to get all the saved products. <br/>The `query_` function is very similar to `query` the difference is that this one does not receive values to replace in the query. <br/>Also, the result of this query is interpreting as an `IO [Product]` type and `liftIO` is a function that take a `IO a` and transform to `m a` value, where `m` is another Monad. That is, it transforms `IO [Product]` result of `query_` into `ActionM [Product]`, that is why the `::` operator is using here, to give information to Haskell about final type. |
| :----: | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

## HTML API
