# Products API

This is an example of API REST built with [Scotty]() a web framework of Haskell and [PostgreSQL]() a relational database. It's a simple API to manage products.

## Index

- [Products API](#products-api)
  - [Index](#index)
  - [Quick run instructions](#quick-run-instructions)
  - [Running PostgreSQL](#running-postgresql)
  - [JSON API](#json-api)
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

## HTML API
