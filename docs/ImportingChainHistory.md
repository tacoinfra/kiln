# Importing Chain History to the Kiln Node from Another Tezos Node

To bake with Kiln, the Kiln Node must be fully synced with the blockchain. Rather than waiting for the Kiln Node to sync from genesis, it is possible to copy chain history from another node. The import process is dependent upon how you are running Kiln. Follow the instructions below specific to your distribution.

## Built from source

1. Start the Kiln Node. You can do so through the Kiln Node’s options menu. Wait for it to finish "Initializing".

2. Stop the Kiln Node. You can do so through the Kiln Node’s options menu.

3. Remove the Kiln Node’s existing context and store

```shell
rm -rf app/.kiln/tezos-node/[chain-id]/context
rm -rf app/.kiln/tezos-node/[chain-id]/store
```

_Note: The chainId for mainnet is `NetXdQprcVkpaWU`_

4. Stop the node whose data you are copying.

5. Copy the context and store folders from the synced node into the Kiln Node’s folder. `~/.tezos-node` is the default location for node data:

```shell
cp -r ~/.tezos-node/context ~/.tezos-node/store app/.kiln/tezos-node/[chain-id]/
```

6. Restart the Kiln Node. As it starts, it should recognize the chain data and update its head block level.


## Debian Distribution

1. Start the Kiln Node. You can do so through the Kiln Node’s options menu. Wait for it to finish "Initializing".

2. Stop Kiln: `sudo systemctl stop kiln`

3. Remove the Kiln Node's existing context and store:

```shell
sudo rm -rf /var/lib/kiln/data-dir/tezos-node/[chain-id]/context
sudo rm -rf /var/lib/kiln/data-dir/tezos-node/[chain-id]/store
```

4. Stop the node whose data you are copying.

5. Copy data and set file permissions:

```shell
sudo cp -r ~/.tezos-node/context ~/.tezos-node/store /var/lib/kiln/data-dir/tezos-node/[chain-id]/
sudo chown -R kiln:kiln /var/lib/kiln/data-dir/tezos-node/[chain-id]
```

6. Restart Kiln: `sudo systemctl start kiln`

## Virtual Machine

1. Start the Kiln Node. You can do so through the Kiln Node’s options menu. Wait for it to finish "Initializing".

2. Stop Kiln: `sudo systemctl stop kiln`

3. Remove the Kiln Node's existing context and store:

```shell
sudo rm -rf /home/kiln/app/.kiln/tezos-node/[chain-id]/context
sudo rm -rf /home/kiln/app/.kiln/tezos-node/[chain-id]/store
```

4. Stop the node whose data you are copying.

5. Copy data:

```shell
sudo cp -r ~/.tezos-node/context ~/.tezos-node/store /home/kiln/app/.kiln/tezos-node/[chain-id]/
```

6. Restart Kiln: `sudo systemctl start kiln`
