# Chain Reset Procedure

Occassionally babylonnet and zeronet are reset, starting over from the genesis block.
When that happens, it is important to clear all the data of the previous chain.

## Tezos Data

Delete the node and client data. By default these are stored at `~/.tezos-node` and
`~/.tezos-client`.

## Kiln Data

Delete Kiln's node data, stored in `./app/.kiln`.

## Reset HWM of Ledger Device

Can be done either through the Kiln's GUI interface or with the following command

  `tezos-client set ledger high watermark for "ledger://<tz...>/" to <HWM>`
