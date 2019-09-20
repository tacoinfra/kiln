# Running a Pre-Built Monitor (Docker Image)

Obsidian Systems provides pre-built Docker images for each release on [Docker Hub](https://hub.docker.com/r/obsidiansystems/tezos-bake-monitor/). These images allow anyone to run the software without building it themselves. It has been tested on Linux and macOS.

To run the Docker image you need to have [Docker](https://www.docker.com/get-started) installed.

Before you can download and run the monitor, you'll need a [PostgreSQL](https://www.postgresql.org/) database that the monitor can use for storage.

The easiest way to get a database running is with Docker. The following command will download the PostgreSQL Docker image (if it's not already downloaded) and start a database instance in the background on port `5432`. The `DOCKER_CONTENT_TRUST=1` tells Docker to verify the signature of this image to ensure it's from the original creator.

```shell
DOCKER_CONTENT_TRUST=1 docker run --name tezos-monitor-postgres -p 5432:5432 -e POSTGRES_PASSWORD=mysecretpassword -d postgres
```

(For anything serious you'll want to pick a better password than `mysecretpassword`.)

For more advanced users, we recommend setting up a Docker network, or better yet, using [Docker Compose](https://docs.docker.com/compose/).

You can stop this database by running `docker container stop tezos-monitor-postgres`. You can then remove it with `docker container rm tezos-monitor-postgres` but be aware your database state will be lost.

Now you can download and run the monitor like this:

On Linux and macOS (Docker Toolbox):

```shell
DOCKER_CONTENT_TRUST=1 docker run --network host --rm obsidiansystems/tezos-bake-monitor:0.6.2 --pg-connection="host=localhost port=5432 dbname=postgres user=postgres password=mysecretpassword"
```

On macOS (Docker Desktop for Mac):

```shell
DOCKER_CONTENT_TRUST=1 docker run -p 8000:8000 obsidiansystems/tezos-bake-monitor:0.6.2 --pg-connection="host=host.docker.internal port=5432 dbname=postgres user=postgres password=mysecretpassword"
```

Replace `mysecretpassword` with your *actually secret* password.

Now open a browser and navigate to `http://localhost:8000` to start configuring your monitor! Instructions can be found below in [Initial Setup](#initial-setup).

Check out `docker run --rm obsidiansystems/tezos-bake-monitor:0.6.2 --help` for more command-line options. For example, you can run the monitor on alphanet by passing `--network=alphanet`.

## Updating an older Docker container

Updating to a newer pre-built Docker image is simple because all your data is stored separately in the PostgreSQL database.

### Making a backup of your data

Ideally you should make a backup of your database before upgrading, just in case something goes wrong.

If you started your PostgreSQL database in Docker (as described above) you can use `docker commit` to save a copy of you current database before the upgrade:

```shell
docker commit tezos-monitor-postgres tezos-monitor-postgres:backup1
```

Use `docker image ls` to see your backup image listed. `docker image rm tezos-monitor-postgres:backup1` will delete it.

Alternatively, if you have a compatible version of `pg_dump` installed, you can make a more lightweight backup by connecting to your database:

```shell
pg_dump "host=host.docker.internal port=5432 dbname=postgres user=postgres password=mysecretpassword" > tezos-monitor-postgres-backup1.sql
```

### Running the newer version

Now you can simply run the newer version. It will automatically migrate your database. Refer to [Running a Pre-Built Monitor](#running-a-pre-built-monitor) for instructions, replacing version numbers where necessary. For example, when you see

```shell
DOCKER_CONTENT_TRUST=1 docker run --network host --rm obsidiansystems/tezos-bake-monitor:0.6.2 ...
```

you can replace `0.6.2` with another available version.

You can remove old images and containers for the monitor safely. All your data is kept in the PostgreSQL instance.
