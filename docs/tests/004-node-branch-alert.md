# Node is on a Branch Alert

## Intent

This alert should appear whenever a monitored node's head becomes a fork of the latest head that the monitor has seen. This means that the alert can only happen when monitoring *more than 1 node* (public nodes count). The monitor considers the "latest head" to be the block with the highest fitness from any node being monitored.

## Tests

  1. Run the monitor with a fresh database. Configure it to monitor a specific network. (`mainnet` is an obvious choice.)
  1. Configure the monitor to send alerts via email or some other medium. Test this to verify it works.
  1. Start the "fragile network sandbox" for the same network. (See instructions below.)
  1. Add three nodes:
      1. `http://localhost:18731`
      1. `http://localhost:18732`
      1. `http://localhost:18733`
      1. Do *not* add any public nodes.
  1. Ensure that all of these nodes are being monitored without any alerts. If any of them have problems, try restarting the sandbox (see instructions below).
  1. Wait for a few minutes as the nodes make progress together. They should all agree on the latest head.
  1. Kill the node using port 18731 like this
      * `kill $(netstat -tnlp | grep 18731 | awk -F '[ /]+' '{print $7}')`
  1. (You will get the "Inaccessible node" alert. But this is not relevant here.)
  1. Wait for a few minutes as the even numbered node (using port `18732`) starts to diverge from the odd numbered one (using port `18733`).
  1. You should eventually see an alert that one of these nodes is on a branch.
  1. Wait longer and you will see that the branch length gets longer and further behind the latest node.
  1. Turn the node on port `18731` back on like this
      * Open the `nix-shell` for the "fragile network sandbox" for the same network. (See instructions below.)
      * Start node 1 again: `tezos-sandbox-fragile-node.sh 1 run --rpc-addr 127.0.0.1:18731`
  1. (The "Inaccessible node" alert should become resolved.)
  1. Wait for a few minutes.
  1. The nodes that were previously on a branch may temporarily switch to being behind (which is still an alert).
  1. The node on a branch should re-align with the other node and the alert should become resolved.


### Starting the Fragile Network

The fragile network works like this:

  1. It starts 9 nodes and a few bakers.
  1. Even numbered nodes (nodes 3, 5, 7, and 9) are all peers.
  1. Odd numbered nodes (nodes 2, 4, 6, and 8) are all peers.
  1. Node 1 has both odd numbered and even numbered nodes as peers.
  1. Because of this, node 1 is the only way that the even nodes and odd nodes can talk to each other.
  1. By stopping node 1, the even and odd halves become a "split brain" because they never talk to each other. The both progress independently.

To run this network:
  1. You must have the `tezos-baking-platform` repository.
  1. Open a terminal shell and `cd` into this directory.
  1. Kill all running nodes and delete any existing sandbox data, like this
      * `pkill tezos-node; pkill tezos-client; rm -rf sandbox/`
  1. Pick a network (`mainnet`, `betanet`, `alphanet`, `zeronet`, or `master`). We'll call it `NETWORK`.
  1. Start a `nix-shell` for the network's sandbox scripts like this
      * `nix-shell -A tezos.NETWORK.sandbox` (replacing `NETWORK` with the one you want)
  1. Start the fragile network like this
      * `export NODE_EXE=$(which tezos-sandbox-fragile-node.sh); tezos-sandbox-theworks.sh`
  1. Wait a minute for the network to start up and stabalize.
  1. The terminal should continuously see output.
  1. If you see an error that looks like the snippet below, you can safely ignore it. The network is still running.
      ```
      CallStack (from HasCallStack):
        error, called at src/Main.hs:171:10 in main:Main
      ```
  1. If you see any other kind of error, start this series of steps again. Likely one of the nodes failed to start.
