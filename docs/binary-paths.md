# Binary Paths

## Quick start

Create a file named `binary-paths` in the `config` directory adjacent
to `backend` executable.

```json
{
    "node-path" : "<tezos-node-path>"
    ,"client-path" : "<tezos-client-path>"
    ,"baker-endorser-paths" :
        [["<protocol-hash>","<tezos-baker-path>","<tezos-endorser-path>"]]
}
```

This configuration specifies the respective locations of these
binaries: tezos-<node,client,baker,endorser>. Notice that the protocol
hash of the desired network must also be included.
