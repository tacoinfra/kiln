# Overview

[Obsidian Systems’](https://obsidian.systems/) Monitoring Software, Kiln, provides individuals running Tezos nodes and bakers with a locally hosted graphical interface enabling easy and effective monitoring. The dashboard displays a tile with relevant information for all nodes and bakers it is monitoring and it alerts the user of any issues that may be encountered. Kiln can also run a Tezos node.

Kiln alerts users within the GUI if a Monitored Node:

* Is on the wrong network
* Is not on the fittest branch
* Falls behind the current head block level
* Cannot be reached by the Monitoring Software (e.g. is offline)
* Reports fewer than a specified number of active peer connections

Or if a Monitored Baker:

* Misses a baking or endorsing opportunity
* Has been deactivated due to inactivity or will be within one cycle

Or if the monitored network:

* Has an update pushed. For instance, if a user is monitoring mainnet with Kiln, they will be notified is mainnet is updated.

In addition to these in-app alerts, users can configure Kiln to send Telegram alerts or use their SMTP Mail Server to send alerts to the email addresses of their choice.

This version (v0.4.0) is an early version of Kiln. Near-term improvements include, but are not limited to:
* Expanding baker monitoring
* Baking from Kiln's GUI

We encourage users to join our Baker Slack (by emailing us for an invite at tezos@obsidian.systems) to provide feedback and let us know what improvements you’d like to see next!

#### Running a node

This Monitor assumes that you are running at least one Tezos node. Options for running a node include:

* Building from Source - The best place to start is [Tezos’ Documentation](http://tezos.gitlab.io/master/introduction/howtoget.html#build-from-sources). There are also several community guides, some of which you can find [here](https://docs.google.com/document/d/1iu-5j8vnnK00-t0CIQcbMDSz5PbEHI09YsKp8utEaj0/edit).
* Using Docker - The docker image for Tezos can be found on [DockerHub](https://hub.docker.com/r/tezos/tezos/). They also provide a [simple script](http://tezos.gitlab.io/master/introduction/howtoget.html#docker-images) for retrieving the images. We do not yet have instructions on connecting the Tezos node and baking monitor Docker containers, but you can either set this up yourself or connect our Docker container to a node you built from source.
* Using Obsidian’s Tezos Baking Platform - See the Tezos Baking Platform's [UsingTezos.md](https://gitlab.com/obsidian.systems/tezos-baking-platform/blob/develop/UsingTezos.md) for instructions.

# Running a Pre-Built Monitor

Obsidian Systems provides pre-built Docker images for each release on [Docker Hub](https://hub.docker.com/r/obsidiansystems/tezos-bake-monitor/). These images allow anyone to run the software without building it themselves. It has been tested on Linux and macOS.

To run the Docker image you need to have [Docker installed](https://www.docker.com/get-started).

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
DOCKER_CONTENT_TRUST=1 docker run --network host --rm obsidiansystems/tezos-bake-monitor:0.4.0 --pg-connection="host=localhost port=5432 dbname=postgres user=postgres password=mysecretpassword"
```

On macOS (Docker Desktop for Mac):

```shell
DOCKER_CONTENT_TRUST=1 docker run -p 8000:8000 obsidiansystems/tezos-bake-monitor:0.4.0 --pg-connection="host=host.docker.internal port=5432 dbname=postgres user=postgres password=mysecretpassword"
```

Replace `mysecretpassword` with your *actually secret* password.

Now open a browser and navigate to `http://localhost:8000` to start configuring your monitor! Instructions can be found below in [Initial Setup](#initial-setup).

Check out `docker run --rm obsidiansystems/tezos-bake-monitor:0.4.0 --help` for more command-line options. For example, you can run the monitor on alphanet by passing `--network=alphanet`.

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
DOCKER_CONTENT_TRUST=1 docker run --network host --rm obsidiansystems/tezos-bake-monitor:0.4.0 ...
```

you can replace `0.4.0` with another available version.

You can remove old images and containers for the monitor safely. All your data is kept in the PostgreSQL instance.

# System Requirements

## For Monitoring

**Disk:** The monitor uses PostgreSQL for *all* storage. The entire database typically uses about 1-2GB.

**Memory:** Idle memory usage is typically under 1GB. When initializing history (usually right after start-up or adding your first node) memory can spike to about 3GB for a short time.

**CPU:** Running with at least 2 cores is recommended.

## For Running a Node

**Disk:** A node running in Kiln will sync with the Tezos blockchain, which is currently ~70GB.

**Memory:** Recommendated RAM for running a Tezos Node is 8GB. We make the same recommendations when running a node in Kiln. *If you are using our Docker Image, note that Docker's default memory allocation is 2GB.* Increasing this allocation to the recommended 8GB is highly recommended.

**CPU**: Running with at least 2 cores is recommended.

# Building from Source

## Prerequisites

These builds have only been tested on Linux.

### Configuring the Nix cache (Recommended)

If you have not done so already, we recommend you add our Nix caches to your Nix configuration to drastically reduce your build time. Please see instructions in [Tezos Baking Platform](https://gitlab.com/obsidian.systems/tezos-baking-platform/blob/develop/README.md).

### Cloning the repository

```shell
git clone https://gitlab.com/obsidian.systems/tezos-bake-monitor.git
cd tezos-bake-monitor/
```

By default you will be on the `develop` branch which is the latest unstable version. For a stable version, checkout `master` or one of the specific version tags.

## Running the build

```shell
mkdir app
```

Then run this command to link the build’s path to the app directory.

```shell
ln -sf $(nix-build tezos-bake-central -A exe --no-out-link)/* app/
```

### Starting the monitor

To run the monitor, enter the `app` directory and start the `backend`. Replace `<network>` with your desired Tezos network, e.g. `zeronet`, `alphanet`, `mainnet`, or with a specific chain ID.

```shell
cd app
./backend --network <network>
```

If you completed these steps correctly, your Monitor should now be running at http://127.0.0.1:8000.

### Updating from an older source build

To update your source build, simply checkout the newer version and `git pull`. For example:

```shell
git checkout master
git pull
```

Then follow the steps in [Running the build](#running-the-build). However, you'll already have an `app` directory. Deleting it would remove your database as well since your database is stored in `app/db`. You can simply overwrite the necessary application files by rerunning the `ln` command.

```shell
ln -sf $(nix-build tezos-bake-central -A exe --no-out-link)/* app/
```

## Building the Docker image

To build the Docker image you must be running on Linux or have at least one Linux remote builder configured.

```shell
nix-build -A dockerImage --no-out-link
```

The result of this command will be the path to a Docker image. You can load it with `docker load -i <path>`.

# Initial Setup

### Running a node

Click *Add Node* in the left panel. Then click *Start Node* on the left side of the modal. Kiln will generate an identity for a local node with the RPC port 8732 and immediately begin syncing with the blockchain. If this port is already in use, Kiln will have difficulty monitoring this node. The Kiln Node can be stopped or restarted through the options menu on the Node's tile on the Dashboard.

### Adding monitored nodes

Click *Add Node* from the left panel and enter the address of the node you would like to monitor under *Connect via address*. Then click *Add Node*. For example, if you would like to add a local node with an RPC interface on the default port of `8732`, you can enter `http://localhost:8732`\*. You can add any node URL to which you know the RPC address. If you do not know the RPC address of the node or if the node wasn't started with `--rpc-addr`, the monitor will not be able to retrieve information from the node.

\* If you're running the monitor from Docker on macOS, `localhost` will not point to your *host*'s network. Instead you can use `host.docker.internal` instead of `localhost`, e.g. `http://host.docker.internal:8732`. `localhost` will also not work on Linux if you run the container without `--network host`, but you can't use `host.docker.internal` in this case.

Once you’ve added at least one node or Public Node, the Dashboard will show you statistics.

### Adding public nodes

Click *Add Node* from the left panel and click one of the tiles under *Connect to a Public Node*. Clicking again will disable the Public Node.

### Adding a baker

Click *Add Baker* from the left panel and input the public key hash (PKH) of the baker you would like to monitor. Kiln will then use Monitored Nodes to gather information about that baker from the blockchain. This initial query can take up to a few hours, and requires the user to monitor at least one node.

### Configuring Telegram notifications

Click *Settings* from the left panel then click *Connect Telegram* and follow the instructions in the popup.

### Configuring email notifications

Click *Settings* from the left panel and provide the SMTP configuration for your SMTP server in the form under *Email*. Add an email address to receive alerts and click *Save Settings*.
