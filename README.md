# Overview

Kiln, by [Obsidian Systems](https://obsidian.systems/), is a tool for both baking and monitoring on the Tezos network. It provides a locally hosted graphical interface, binaries for tezos-client, tezos-node, tezos-baker, and tezos-endorser, and it builds a cache of chain data from the nodes to which it connects.

## System Requirements

System requirements are dependent on whether you plan on running a Tezos node in Kiln, which is necessary for baking.

*Note: If you are running a node using our Docker Image, note that Docker's default memory allocation is 2GB. Increasing this to 8GB is highly recommended.*

### System Requirements For Monitoring

**Disk:** The monitor uses PostgreSQL for *all* storage. The entire database typically uses about 1-2GB.

**Memory:** Idle memory usage is typically under 1GB. When initializing history (usually right after start-up or adding your first node) memory can spike to about 3GB for a short time.

**CPU:** Running with at least 2 cores is recommended.

### System Requirements For Baking

Kiln bakes with a local node, which increases system requirements.

**Disk:** A node running in Kiln will sync with the Tezos blockchain, which is currently ~70GB. **SSD is highly recommended over HHD.**

**Memory:** Recommendated RAM for running a Tezos Node is 8GB. We recommend at least 10GB RAM to account for the node, baker, endorser, and Kiln’s processes.

**CPU**: Running with at least 2 cores is recommended.

# Obtaining Kiln

Kiln can be built from source on linux distributions and Obsidian Systems provides:
* pre-built Docker images hosted on [Docker Hub](https://hub.docker.com/r/obsidiansystems/tezos-bake-monitor/).
* pre-built deb files

This table details Kiln's distributions, their key features, and their availability. Click a link in the left column to learn more about that distribution of Kiln.

| **Distribution**                                       | **Supports Baking?** | **Operating Systems**           | **Released?**     |
|--------------------------------------------------------|----------------------|---------------------------------|-------------------|
| [Build from Source](docs/distros/build-from-source.md) | Yes                  | Linux                           | Yes               |
| [Docker](docs/distros/docker.md)                       | **No**               | Linux / Mac                     | Yes               |
| [Linux Distribution](docs/distros/ubuntu.md) (.deb)    | Yes                  | Ubuntu (others soon)            | Yes               |
| VM Package (OVA)                                       | Yes                  | Any                             | Est. May 2019     |
| Mac Distribution                                       | Yes                  | Mac                             | Est. May 2019     |

# Using Kiln to Bake

Kiln, in conjunction with [Tezos Baking for the Ledger Nano S](https://github.com/obsidiansystems/ledger-app-tezos), can be used to bake. Kiln runs the node, baker, and endorser locally while monitoring them to notify the user of common issues and events.

Baking requires the Kiln node to be fully synced with the Tezos blockchain. Once the node is synced, click ‘Add Baker’ in the left sidebar. Then click ‘Start Baking’ and follow the instructions provided.

Once you’ve registered as a delegate, it will take time for you to earn baking and endorsing rights. As soon as Kiln detects you have rights, your next right will be displayed.


# Using Kiln as a Monitor

Kiln monitors nodes, bakers, and the Tezos network to keep bakers fully informed. The dashboard displays a tile with relevant information for all the nodes and bakers it is monitoring, and it alerts the user of any issues that it may encounter.

### Network Monitoring

Kiln produces a notification if an update is pushed to the Tezos network. For instance, if a user is monitoring mainnet with Kiln, they will be notified if mainnet is updated.

### Node Monitoring

Kiln produces a notification if a monitored node:

* Is on the wrong network
* Is not on the fittest branch
* Falls behind the current head block level
* Cannot be reached by the Monitoring Software (e.g. is offline)
* Reports fewer than a specified number of active peer connections

### Baker Monitoring

Kiln produces a notification if a monitored baker:

* Misses a baking or endorsing opportunity
* Is accused of double baking or double endorsing
* Has been deactivated due to inactivity or will be within one cycle

*Note: To monitor a baker, you must be monitoring a node. Public Nodes provide general information about the network, but they do not provide full block history or baking and endorsing rights, which are required for monitoring a baker.*

### Notification Pathways

In addition to in-app alerts, users can configure Kiln to send Telegram alerts or use their SMTP Mail Server to send alerts to the email addresses of their choice. Notification pathways can be configured on the Settings page.

For more information, see notes on configuring [Telegram](#configuring-telegram-notifications) and [Email](#configuring-email-notifications) notifications below.

# Initial Setup

### Running a node

Click *Add Node* in the left panel. Then click *Start Node* on the left side of the modal. Kiln will generate an identity for a local node with the RPC port 8732 and immediately begin syncing with the blockchain. If this port is already in use, Kiln will have difficulty monitoring this node (the port can be modified by [command line options](#command-line-options)). The Kiln Node can be stopped or restarted through the options menu on the Node's tile on the Dashboard.

### Adding monitored nodes

Click *Add Node* from the left panel and enter the address of the node you would like to monitor under *Monitor via Address*. Then click *Add Node*. For example, if you would like to add a local node with an RPC interface on the default port of `8732`, you can enter `http://localhost:8732`\*. You can add any node URL to which you know the RPC address. If you do not know the RPC address of the node or if the node wasn't started with `--rpc-addr`, the monitor will not be able to retrieve information from the node.

\* If you're running the monitor from Docker on macOS, `localhost` will not point to your *host*'s network. Instead you can use `host.docker.internal` instead of `localhost`, e.g. `http://host.docker.internal:8732`. `localhost` will also not work on Linux if you run the container without `--network host`, but you can't use `host.docker.internal` in this case.

Once you’ve added at least one node or a Public Node, the Dashboard header will show the network status.

*Note: This assumes that you are running at least one Tezos node. Options for running a node include:*

* *Building from Source - The best place to start is [Tezos’ Documentation](http://tezos.gitlab.io/master/introduction/howtoget.html#build-from-sources). There are also several community guides, some of which you can find [here](https://docs.google.com/document/d/1iu-5j8vnnK00-t0CIQcbMDSz5PbEHI09YsKp8utEaj0/edit).*
* *Using Docker - The docker image for Tezos can be found on [DockerHub](https://hub.docker.com/r/tezos/tezos/). They also provide a [simple script](http://tezos.gitlab.io/master/introduction/howtoget.html#docker-images) for retrieving the images. We do not yet have instructions on connecting the Tezos node and baking monitor Docker containers, but you can either set this up yourself or connect our Docker container to a node you built from source.*
* *Using Obsidian’s Tezos Baking Platform - See the Tezos Baking Platform's [UsingTezos.md](https://gitlab.com/obsidian.systems/tezos-baking-platform/blob/develop/UsingTezos.md) for instructions.*

### Adding public nodes

Click *Add Node* from the left panel and click one of the tiles under *Connect to a Public Node*. Clicking again will disable the Public Node.

### Monitoring a baker

Click *Add Baker* from the left panel and input the public key hash (PKH) of the baker you would like to monitor. Kiln will then use Monitored Nodes to gather information about that baker from the blockchain. This initial query can take up to a few hours.

*Note: To monitor a baker, you must be monitoring a node - public nodes are not sufficient. Public Nodes provide information about the current head block, but they do not provide full block history of baking and endorsing rights.*

### Configuring Telegram notifications

Click *Settings* from the left panel then click *Connect Telegram* and follow the instructions in the popup. Telegram notifications can be sent to a group or directly to a single user.

### Configuring email notifications

Click *Settings* from the left panel and provide the SMTP configuration for your SMTP server in the form under *Email*. Add an email address to receive alerts and click *Save Settings*.

# Troubleshooting

### Known Issue: Front End Stops Updating

There are instances where Kiln’s front end will stop updating, but the backend will continue functioning properly. For instance, resolving a notification will not cause the notification to disappear until the page is refreshed. Stopping and restarting Kiln fixes this issue.

# Contact Us

We encourage users to join our Baker Slack (by emailing us for an invite at tezos@obsidian.systems) to provide feedback and let us know what improvements you’d like to see next!
