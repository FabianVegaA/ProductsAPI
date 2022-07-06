# Products API

This is a example of API REST made with [Scotty]() in Haskell. It's a simple API to manage products. The data is stored in a [PostgreSQL]() database.

> Before to start, you need to install have stack and docker-compose installed.

For running the API, you need to run the following commands:

```bash
$ stack run
$ docker-compose up # or sudo docker-compose up
```

---

## Index

- [Products API](#products-api)
  - [Index](#index)
  - [Running PostgreSQL](#running-postgresql)
  - [JSON API](#json-api)
  - [HTML API](#html-api)

## Running PostgreSQL

The tutorial try easy to understand, perhaps if you know how to use docker-compose and use a PostgreSQL database, you can skip this section.

The first step is make a docker-compose.yml file.

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

The `example.env` file is a file with the following content:

```environment
POSTGRES_USER=postgres
POSTGRES_PASSWORD=123456
POSTGRES_DB=products
```

This file is used to configure the database.



## JSON API

## HTML API
