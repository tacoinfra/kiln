# Chain Reset Procedure 

Occassionally alphanet and zeronet are reset, starting over from the genesis block.
When that happens, it is important to clear all old data from the previous chain. 

## Tezos Data

Delete the node and client data. By default, these are stored at `~/.tezos-node` and
`~/.tezos-client`.

## Kiln Data

Delete Kiln's database, located at `/app/db`.