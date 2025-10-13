#!/bin/bash

# docker run --name pg_container -e POSTGRES_PASSWORD=12345 -e POSTGRES_USER=postgres -e POSTGRES_DB=db -d -p 5432:5432 -v pgdata:/var/lib/postgresql/data postgres

# psql -h localhost -p 5432 -U postgres -W -d db

CONTAINER_NAME="pg_container"
POSTGRES_USER="postgres"
POSTGRES_PASSWORD="12345"
POSTGRES_DB="db"
HOST_PORT=5432
CONTAINER_PORT=5432
VOLUME_NAME="pgdata"
IMAGE_NAME="postgres"

docker run --name "$CONTAINER_NAME" \
  -e POSTGRES_USER="$POSTGRES_USER" \
  -e POSTGRES_PASSWORD="$POSTGRES_PASSWORD" \
  -e POSTGRES_DB="$POSTGRES_DB" \
  -p "$HOST_PORT:$CONTAINER_PORT" \
  -v "$VOLUME_NAME":/var/lib/postgresql/data \
  -d "$IMAGE_NAME"

psql -h localhost -p "$HOST_PORT" -U "$POSTGRES_USER" -W -d "$POSTGRES_DB"

