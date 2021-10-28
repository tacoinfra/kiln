# Caching Network and Baker Data in Kiln
One important part of Kiln is providing the user an idea of how their node is operating within the context of the Tezos network. For instance, at any given time the baker will want to know “is my node in sync with the network?” If it is not, the baker will miss their opportunity to bake or endorse a block for as long as they are out of sync.

Once crude way to monitor this is time-based notifications. Assuming no priority 0 blocks are missed, Tezos mainnet sees a new block every 60 seconds, so you could assume that if you haven’t seen a new block in roughly 60 seconds you may be falling behind. This falls short of ideal and would trigger false positives anytime a priority 0 baker missed their opportunity or the network as a whole stalled. Instead, Kiln takes a more sophisticated approach where notifications are based on the state of the network as determined from several data sources.
## Data Sources and Discovery
Kiln learns about the state of the Tezos network and builds a cache of this information by monitoring nodes chosen through the user’s configuration. These can include the Kiln Node, nodes the user has chosen to monitor via their IP address and port.

The cache Kiln builds from these data sources is powerful: it offers the highest guarantees of accuracy, allows Kiln to acknowledge states where monitored nodes recognize multiple chain heads, spot reorganizations, and corrects itself if its cached history is found to be incorrect due to a reorganization. It also triggers notifications when the Kiln Node or a monitored node is 1) on the wrong network, 2) unsycned with the network, or 3) behind the current head block level. The full list of node notifications can be found [here](../README.md#node-monitoring).
## Baker Data Discovery
The cache Kiln builds also powers features and notifications about the baker. For instance, it:

* compares rights with history to determine if the baker has missed any opportunities
* checks block history for accusations against the Kiln Baker to detect if they have been accused of double baking or double endorsing
* Stores information about the current amendment period so the baker can see which proposal have been proposed, how the Kiln baker and other bakers are voting, and whether the proposed amendment is likely to become mainnet

This information can also be used to develop new features such as a table of rights and history or to calculate the baker’s efficiency.

Any node - the Kiln Node, monitored nodes, and public data sources - are candidates for gathering information about the user’s baker and network context to build Kiln’s cache, however these nodes may also be used for more important tasks such as baking. Requesting a large amount of information from a node used by a baker can result in missed opportunities, so Kiln spaces out RPC requests so as to not interfere with more critical processes. As a result it takes longer to cache chain history than if this were not a consideration.

## The Introduction of Snapshots and History Modes
When archive nodes were the only node type, all data sources could be treated equally (with some exceptions, e.g. that the TzScan API did not provide all the same data as the Tezos node and Tezos Foundation Nodes did not whitelist all RPC endpoints). This changed with the introduction of snapshots and history modes.

[History modes and snapshots](https://research-development.nomadic-labs.com/introducing-snapshots-and-history-modes-for-the-tezos-node.html) allow the node to run under a more lightweight configuration or to start syncing from a given block level rather than from genesis. Kiln took advantage of both of these features in [v0.6.0](https://medium.com/kiln/kiln-v0-6-0-snapshots-full-nodes-and-revamped-public-nodes-241e8baf4956) by running the Kiln node in the full history mode by default, mirroring the default setting in tezos-client. As archive nodes cannot be started from a snapshot, this also allowed Kiln to support snapshot imports, dramatically reducing the time it takes to sync.

However, changing the Kiln node from archive to full history mode meant we could no longer guarantee Kiln had access to an archive node and, consequently, there was no guarantee Kiln would have access to all the information needed to support its features. For instance:

* A node from a snapshot that is too recent will not contain all the governance data of the current amendment cycle.
* Full nodes only store baking rights from the last 5 cycles, so Kiln would have no prior history for a baker who recently moved to Kiln after baking via other means.
