# Configuration

Kiln configuration can either be done by passing command line arguments (e.g.
`--network babylonnet`) or by putting a config file with the argument contents
into a file in the config directory (e.g. a file `config/network` containing
`babylonnet`). Using config files is required during development since `ob run`
doesn't allow passing of command line arguments.

## loggers

See [logging.md](https://gitlab.com/tezos-kiln/kiln/-/blob/develop/docs/logging.md)

## pg-connection CONNSTRING

Connection string or URI to PostgreSQL database. If blank, use connection string
in 'db' file or create a database there if empty.

## route URL

Root URL for this service as seen by external users. If blank, use contents of
'config/route'.

## email-from EMAIL

Email address to use for 'From' field in email notifications. If blank, use
contents of 'config/email-from'.

## check-for-upgrade BOOL

Enable/disable upgrade checks. If blank, use contents of
'config/check-for-upgrade'. If that is blank, default to enabled.

## upgrade-branch BRANCH

Upstream Git branch to use for checking upgrades. If blank, use contents of
'config/upgrade-branch'. If that is blank, default to 'master'.

## network NETWORK

Name of a network (mainnet, babylonnet, zeronet) or a network ID to monitor. If
blank, use contents of 'config/network'. If also blank, default to 'mainnet'.

## tzscan-api-uri URL

Custom tzscan API URL.  Default none.

## blockscale-api-uri URL

Custom Blockscale API URL.  Default none.

## nodes URIS

Force the set of monitored nodes to be exactly the given set of
(comma-separated) list of nodes. If given multiple times, the sets will be
unioned. Defaults to off.

## bakers PUBLICKEYHASHES

Force the set of monitored bakers to be exactly the given set of
(comma-separated) list of bakers. If given multiple times, the sets will be
unioned. Defaults to off.

## network-gitlab-project-id PROJECTID

The GitLab project id to query for network updates. Defaults to off.

## kiln-node-rpc-port PORT

The RPC port to use for the kiln node. Defaults to 8733.

## kiln-node-net-port PORT

The port where kiln node accepts peer-to-peer connections. Defaults to 9733.

## kiln-data-dir DIRECTORY

The data directory used by the kiln node and tezos-client. Defaults to
"./.kiln".

## kiln-node-custom-args ARGS

Custom arguments for the Kiln Node.

## kiln-baker-custom-args ARGS

Custom arguments for the Kiln baker daemon.

## ledger-check-delay SECONDS

The time between connectivity checks to the ledger. By default this delay is 10 seconds.

## binary-paths

Create a file named `binary-paths` in the `config` directory adjacent
to the `backend` executable.

```json
{
    "node-path" : "<tezos-node-path>",
    "client-path" : "<tezos-client-path>",
    "baker-paths" :
    [
        { "proto" : "<protocol-hash>",
          "baker-path" : "<tezos-baker-path>"
        }
    ]
}
```

This configuration specifies the respective locations of these
binaries: tezos-<node,client,baker>. Notice that the protocol
hash of the desired network must also be included.
