# Importing Chain History to the Kiln Node from Another Tezos Node

To bake with Kiln, the Kiln Node must be fully synced with the blockchain. Rather than waiting for the Kiln Node to sync from genesis, it is possible to copy chain history from another node. The import process is dependent upon how you are running Kiln.

## Built from source

1. Start the Kiln Node so it can generate an identity. You’ll know the Kiln Node has generated an identity when it shows block level 0 in the UI. 

2. Stop the Kiln Node. You can do so through the Kiln Node’s options menu.

3. Remove the Kiln Node’s existing context and store

`rm -rf /tezos-bake-monitor/app/.kiln/tezos-node/[chainId]/context`
`rm -rf /tezos-bake-monitor/app/.kiln/tezos-node/[chainId]/store`

4. Stop the node whose data you are copying.

5. Copy the context and store folders from the synced node into the Kiln Node’s folder. They can be found in the .tezos-node folder

`cp -r ~/.tezos-node/context /tezos-bake-monitor/app/.kiln/tezos-node/[chainId]`
`cp -r ~/.tezos-node/store /tezos-bake-monitor/app/.kiln/tezos-node/[chainId]`

_Note: The `chainId` for mainnet is `NetXdQprcVkpaWU`_

6. Restart the Kiln Node. As it starts, it should recognize the chain data and update its head block level.

## Debian Distribution

1. Start the Kiln Node so it can generate an identity. You’ll know the Kiln Node has generated an identity when it shows block level 0 in the UI. 

2. Stop Kiln: `sudo systemctl stop kiln`

3. Remove the Kiln Node's existing context and store:

`sudo rm -rf /var/lib/kiln/data-dir/tezos-node/[chainId]/context`
`sudo rm -rf /var/lib/kiln/data-dir/tezos-node/[chainId]/store`

4. Stop the node whose data you are copying

5. Copy new data. Be sure you have stopped the node whose data you are copying first.

`sudo cp -r ~/.tezos-node/context /var/lib/kiln/data-dir/tezos-node/[chainId]/`
`sudo cp -r ~/.tezos-node/store /var/lib/kiln/data-dir/tezos-node/[chainId]/`
`sudo chown -R kiln:kiln /var/lib/kiln/data-dir/tezos-node/[chainId]/context`
`sudo chown -R kiln:kiln /var/lib/kiln/data-dir/tezos-node/[chainId]/store`

6. Restart Kiln: `sudo systemctl start kiln`
