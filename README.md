# Overview

[Obsidian Systems’](https://obsidian.systems/) Monitoring Software provides individuals running Tezos nodes with a locally hosted graphical interface, enabling easy and effective node monitoring. In addition to the nodes they manage (‘Monitored Nodes’), users can also elect to view information from ‘Public Nodes’ managed by Obsidian Systems, the Tezos Foundation, and OCamlPro (tzscan.io).

The Software alerts users within the GUI if a Monitored Node:

* Is on the wrong network
* Is on the wrong branch
* Falls behind the current head block level
* Cannot be reached by the Monitoring Software (e.g. is offline)

In addition to these in-app alerts, users can connect their SMTP Mail Server to send alerts to the email addresses of their choice.

This version (v0.1) is a very early version of our Monitoring Software. Near-term improvements include, but are not limited to:
* Improving the UI and user-flow
* Expanding to monitoring bakers
* Introducing new alert pathways

We encourage users to join our Baker Slack (by emailing us for an invite at tezos@obsidian.systems) to provide feedback and let us know what improvements you’d like to see next!

#### Running a Node

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

On Linux:

```shell
DOCKER_CONTENT_TRUST=1 docker run --network host --rm obsidiansystems/tezos-bake-monitor:0.1 --pg-connection="host=localhost port=5432 dbname=postgres user=postgres password=mysecretpassword"
```

On macOS:

```shell
DOCKER_CONTENT_TRUST=1 docker run -p 8000:8000 obsidiansystems/tezos-bake-monitor:0.1 --pg-connection="host=host.docker.internal port=5432 dbname=postgres user=postgres password=mysecretpassword"
```

Replace `mysecretpassword` with your *actually secret* password.

Now open a browser and navigate to `http://localhost:8000` to start configuring your monitor! Instructions can be found below in [Initial Setup](#initial-setup).

Check out `docker run --rm obsidiansystems/tezos-bake-monitor:0.1 --help` for more command-line options. For example, you can run the monitor on alphanet by passing `--network=alphanet`.

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

### Running the Newer Version

Now you can simply run the newer version. It will automatically migrate your database. Refer to [Running a Pre-Built Monitor](#running-a-pre-built-monitor) for instructions, replacing version numbers where necessary. For example, when you see

```shell
DOCKER_CONTENT_TRUST=1 docker run --network host --rm obsidiansystems/tezos-bake-monitor:0.1 ...
```

you can replace `0.1` with another available version.

You can remove the old image and container safely. They do not store any data.


# Known Issues

  * If the frontend page loses connection to the server it will stop showing live data. This might happen if, for example, your computer goes to sleep with the page open. For now, you need to manually refresh the page to reconnect. This will be fixed in a future release.

# Building the Monitor from Source

## Prerequisites

These builds have only been tested on Linux. They previously worked on MacOS, but have not been tested recently.

##### Gitlab SSH Keys

This project is hosted on Gitlab. If you have not already, you should make an account and [set up SSH keys with Gitlab](https://docs.gitlab.com/ee/gitlab-basics/create-your-ssh-keys.html).

##### Setting up Nix Caching (Recommended)

If you have not already, we recommend you setup Nix caching to drastically reduce your build time. Please see instructions in [Tezos Baking Platform](https://gitlab.com/obsidian.systems/tezos-baking-platform/blob/develop/README.md).

##### Cloning the Repo

Clone the Tezos Bake Monitor repo and checkout the develop branch.

```shell
$ git clone https://gitlab.com/obsidian.systems/tezos-bake-monitor.git
$ cd tezos-bake-monitor/
$ git checkout develop
```

The monitoring software uses Git submodules, which allow a Git repository to be kept as a subdirectory of another Git repository. Sync and update Tezos Bake Monitor’s submodules.

```shell
$ git submodule sync
$ git submodule update --recursive --init
```

## Running the Build

Enter the Tezos Bake Central subdirectory:

```shell
$ cd tezos-bake-central/
```

Build the Monitor by running the following command. This also returns a path which you will link to in a couple steps.

```shell
$ nix-build -A exe --out-link result
```

Once complete, still within tezos-bake-monitor/tezos-bake-central, create a directory called ‘app’

```shell
$ mkdir app
```

Then run this command to link the build’s path to the app directory.

```shell
$ ln -s $(nix-build -A exe --no-out-link)/* app/
```

### Starting the Monitor

To run the Monitor, enter the app directory and initiate the backend. Replace <network> with your desired Tezos network, i.e. zeronet, alphanet, mainnet.

```shell
$ cd app
$ ./backend --network <network>
```

If you completed these steps correctly, your Monitor should now be running at http://127.0.01:8000.

# Initial Setup

### Adding Monitored Nodes

When you open the Monitor in your browser, you will be taken to the Options Tab. Under ‘Monitored Nodes’, enter the IP address and port of the node you would like to monitor and click ‘Add Node’. For example, if you would like to add a local node with an RPC interface on the default port of `8732`, you can enter `http://localhost:8732`\*. You can add any node URL to which you know the RPC port. If you do not know the RPC port of the node, the Monitor will not be able to retrieve information from the node.

\* If you're running the monitor from Docker on macOS, `localhost` will not point to your *host*'s network. Instead you can use `host.docker.internal`. For example, `http://host.docker.internal:8732`. This is also true on Linux if you run the container without `--network host`, but you can't use `host.docker.internal` in this case.

Once you’ve added at least one Monitored or Public Node, the Nodes Tab should appear. There you can view information about your Monitored Node(s) alongside the Public Nodes you have also chosen.

### Adding Public Nodes

On the Options Tab, you’ll see a section called ‘Public Nodes’, which lists a button for each Public Node you’re able to observe. By default, none of these are selected. To observe them, toggle on the button for that node. It should then appear on the Node Tab.

### Email Configuration

Within the Options Tab of the Monitor you can configure your own SMTP email server to send alerts if a node is on the wrong chain or network, more than five blocks behind, or has gone offline. To link your email server to the Monitor, enter the location information (host, port, and protocol), as well as the authentication information (username and password). When you have finished filling in the fields, hit ‘Save’ and move on to ‘Notification Recipients’. Add the email addresses of whomever you would like to receive the email alerts.

Once you’ve added a notification recipient, an orange ‘Send Test’ button will appear next to their email address. Pressing that button will send a test email to that address, confirming that the SMTP server has been configured correctly.
