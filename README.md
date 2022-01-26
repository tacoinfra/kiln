# Kiln

Kiln is a tool for both baking and monitoring on the Tezos network. It
provides a locally hosted graphical interface, binaries for
tezos-client, tezos-node, tezos-baker, and tezos-endorser, and it
builds a cache of chain data from the nodes to which it connects.

**For step-by-step instructions on how to get started, see the
Obsidian System's medium post [How to Install Kiln and Bake on
Ubuntu](https://medium.com/@obsidian.systems/how-to-install-kiln-and-bake-on-ubuntu-a13d17df63c).**

**Installation guide for MacOS Catalina or later is available [here](https://medium.com/tezos-kiln/how-to-install-kiln-on-macos-catalina-ce0821f97dcf).**

Past release notes are available
[here](https://medium.com/@obsidian.systems).

If you don't need/want graphical user interface, consider baking
setup with [Serokell Tezos distributions](https://github.com/serokell/tezos-packaging/blob/master/docs/baking.md)
and using [Pyrometer](https://gitlab.com/tezos-kiln/pyrometer) for monitoring.

## Quick Start

- Download latest release from [Releases](https://gitlab.com/tezos-kiln/kiln/-/releases)

### Ubuntu

- Install or update:

    **On desktop:**
    - Double click downloaded .deb file
    - Click `Install` button

    **From the command line:** assuming .deb file
    (e.g. `kiln_0.11.1_amd64.deb`) is downloaded to `~/Downloads` folder
    - Open terminal
    - Run

```
sudo dpkg -i ~/Downloads/kiln_0.11.1_amd64.deb
```

- Go to Kiln web user interface: open <http://localhost:8000> in a web browser

### MacOS

- Install or update:
  - Right click downloaded .pkg file, select "Open With > Installer"
  - Confirm you would like to install this application
  - Follow installer prompts

- After computer reboots, go to Kiln web user interface: open
  <http://localhost:8444> in a web browser

## Troubleshooting

### UI stops updating

There are instances where Kiln’s user interface will stop updating,
but the backend will continue functioning properly. For instance, resolving a
notification will not cause the notification to disappear until the
page is refreshed. Stopping and restarting Kiln fixes this issue.

Restart Kiln (Ubuntu):

```
sudo systemctl stop kiln
sudo systemctl start kiln
```

Restart Kiln (MacOS):

```
launchctl stop tezos.kiln
launchctl start tezos.kiln
```

### Node status in UI is stale

Sometimes Kiln UI may [stop properly updating monitored node
status](https://gitlab.com/tezos-kiln/kiln/-/issues/4).

To verify that Kiln's Tezos node is synchronized with the network run
the following command:

Check node is synced (Ubuntu):

```
/usr/share/kiln/nix/store/*-tezos-*/bin/tezos-client --endpoint http://localhost:8733 bootstrapped
```

Check node is synced (MacOS):

```
export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/kiln-nix/lib
/usr/local/kiln-nix/nix/store/*tezos*/bin/tezos-client --endpoint http://localhost:8733 bootstrapped
```

If node is synchronized with the network, the command will print "Node
is bootstrapped." message and exit immediately.

Or check via node's web API, e.g. using curl:

```
curl http://localhost:8733/chains/main/is_bootstrapped
```

Healthy node should return
```
{"bootstrapped":true,"sync_state":"synced"}
```

Stopping and restarting Kiln typically resolves the issue.

### Missed bake/endorsement
A typical reason for missing a bake or endorsement is unavailable
Ledger device. Examine baker and endorser logs to verify if that's the
case.

Check logs (Ubuntu):

```
# everythin
journalctl -u kiln

# filter messages from baker
journalctl -u kiln | grep baker

# filter message from endorser within a time period
journalctl -u kiln --since "5 days ago" --until "1 hour ago" | grep endorser
```

Consult journalctl
[documentation](https://manpages.ubuntu.com/manpages/jammy/en/man1/journalctl.1.html)
or
[tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-journalctl-to-view-and-manipulate-systemd-logs)
for more ways to browse and filter the logs.

Check logs (MacOS):

- go to `Applications > Utilities` and launch `Console` application.
- Select `Log Reports`
- Type `kiln` in search bar
- Select one of kiln log files

Verify that Ledger is connected (Ubuntu):

```
/usr/share/kiln/nix/store/*-tezos-*/bin/tezos-client --endpoint http://localhost:8733 list connected ledgers
```

Verify that Ledger is connected (MacOS):

```
export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/kiln-nix/lib
/usr/local/kiln-nix/nix/store/*tezos*/bin/tezos-client --endpoint http://localhost:8733 list connected ledgers
```

If ledger is not listed in the output:

- Make sure Tezos baking application is running on the device
- Make sure that device is properly plugged in, on both ends
- Try unplug the device, plug it back in, unlock and start baking app again
- Try different USB port
- Try different USB cable

## System Requirements

System requirements are dependent on whether you plan on running a
Tezos node in Kiln, which is necessary for baking.

*Note: If you are running a node using our Docker Image, note that
Docker's default memory allocation is 2GB. Increasing this to 8GB is
highly recommended.*

### System Requirements For Monitoring

**Disk:** The monitor uses PostgreSQL for *all* storage. The entire
database typically uses about 1-2GB.

**Memory:** Idle memory usage is typically under 1GB. When
initializing history (usually right after start-up or adding your
first node) memory can spike to about 3GB for a short time.

**CPU:** Running with at least 2 cores is recommended.

### System Requirements For Baking

Kiln bakes with a local node, which increases system requirements.

**Disk:** A node running in Kiln will sync with the Tezos blockchain,
which is currently ~70GB. **SSD is highly recommended over HHD.**

**Memory:** Recommendated RAM for running a Tezos Node is 8GB. We
recommend at least 10GB RAM to account for the node, baker, endorser,
and Kiln’s processes.

**CPU**: Running with at least 2 cores is recommended.

## Using Kiln to Bake

Kiln, in conjunction with [Tezos Baking for the Ledger Nano
S](https://github.com/obsidiansystems/ledger-app-tezos), can be used
to bake. Kiln runs the node, baker, and endorser locally while
monitoring them to notify the user of common issues and events.

Baking requires the Kiln node to be fully synced with the Tezos
blockchain. Once the node is synced, click ‘Add Baker’ in the left
sidebar. Then click ‘Start Baking’ and follow the instructions
provided.

Once you’ve registered as a delegate, it will take time for you to
earn baking and endorsing rights. As soon as Kiln detects you have
rights, your next right will be displayed.

## Using Kiln as a Monitor

Kiln monitors nodes, bakers, and the Tezos network to keep bakers
fully informed. The dashboard displays a tile with relevant
information for all the nodes and bakers it is monitoring, and it
alerts the user of any issues that it may encounter.

### Network Monitoring

Kiln produces a notification if an update is pushed to the Tezos
network. For instance, if a user is monitoring mainnet with Kiln, they
will be notified if mainnet is updated.

### Node Monitoring

Kiln produces a notification if a monitored node:

- Is on the wrong network
- Is unsynced with the network
- Falls behind the current head block level
- Cannot be reached by the Monitoring Software (e.g. is offline)

### Baker Monitoring

Kiln produces a notification if a monitored baker:

- Misses a baking or endorsing opportunity
- Is accused of double baking or double endorsing
- Has been deactivated due to inactivity or will be within one cycle

*Note: To monitor a baker, you must be monitoring a node.*

### Notification Pathways

In addition to in-app alerts, users can configure Kiln to send
Telegram alerts or use their SMTP Mail Server to send alerts to the
email addresses of their choice. Notification pathways can be
configured on the Settings page.

For more information, see notes on configuring
[Telegram](#configuring-telegram-notifications) and
[Email](#configuring-email-notifications) notifications below.

## Initial Setup

### Running a node

Click *Add Node* in the left panel. Then click *Start Node* on the
left side of the modal. Kiln will generate an identity for a local
node with the RPC port 8733 and immediately begin syncing with the
blockchain. If this port is already in use, Kiln will have difficulty
monitoring this node (the port can be modified by [command line
options](./docs/distros/build-from-source.md#command-line-options)). The Kiln Node can be stopped or
restarted through the options menu on the Node's tile on the
Dashboard.

### Adding monitored nodes

Click *Add Node* from the left panel and enter the address of the node
you would like to monitor under *Monitor via Address*. Then click *Add
Node*. For example, if you would like to add a local node with an RPC
interface on the default port of `8732`, you can enter
`http://localhost:8732`\*. You can add any node URL to which you know
the RPC address. If you do not know the RPC address of the node or if
the node wasn't started with `--rpc-addr`, the monitor will not be
able to retrieve information from the node.

\* If you're running the monitor from Docker on macOS, `localhost`
will not point to your *host*'s network. Instead you can use
`host.docker.internal` instead of `localhost`,
e.g. `http://host.docker.internal:8732`. `localhost` will also not
work on Linux if you run the container without `--network host`, but
you can't use `host.docker.internal` in this case.

Once you’ve added at least one node, the Dashboard
header will show the network status.

*Note: This assumes that you are running at least one Tezos
node. See [Tezos
documentation](https://tezos.gitlab.io/introduction/howtoget.html) on
various ways of running a node*

### Monitoring a baker

Click *Add Baker* from the left panel and input the public key hash
(PKH) of the baker you would like to monitor. Kiln will then use
Monitored Nodes to gather information about that baker from the
blockchain. This initial query can take up to a few hours.

*Note: To monitor a baker, you must be monitoring a node.*

### Configuring a baker

In case you want to run the baker with some additional arguments, e.g.
you want to run a baker with a vote to end the liquidity baking subsidy,
you should use `--kiln-baker-custom-args` option.

To specify the liquidity baking escape vote you should create a file
`vote.json` with the following contents:
```
{ "liquidity_baking_escape_vote": true/false }
```
And run Kiln with `--kiln-baker-custom-args="--votefile vote.json"` option.

### Configuring Telegram notifications

Click *Settings* from the left panel then click *Connect Telegram* and
follow the instructions in the popup. Telegram notifications can be
sent to a group or directly to a single user.

### Configuring email notifications

Click *Settings* from the left panel and provide the SMTP
configuration for your SMTP server in the form under *Email*. Add an
email address to receive alerts and click *Save Settings*.

## Contact Us

Users can join [Tezos Baking
Slack](https://tezos-kiln.org/joinbakingslack) to provide feedback.
