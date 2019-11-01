# API for Kiln's Internal Cache

Kiln internally maintains a cache of various aspects of the network that it's monitoring. This is for several reasons:

  * To avoid overloading monitored nodes so they can focus on what they do well, syncing with the network.
  * To make complex network queries fast.
  * To offer Kiln users access to cached network data.

If enabled, Kiln will serve this cache via an HTTP API.

> WARNING: These APIs are still under active development and are subject to change.

## Enabling the cache API

Start the Kiln `backend` with `--serve-node-cache=yes` or create a file in `config/serve-node-cache` adjacent to `backend` with the contents of `yes`.

## Endpoints

All endpoints return JSON.

All endpoints require that Kiln is successfully connected to at least one node other than tzscan. Otherwise the result will be HTTP 400 error.

Most endpoints require that Kiln is successfully connected to at least one non-public node. Otherwise the result will be HTTP 400 error.

### Chain ID

Retrieve the *chain ID* of the chain/network being monitored/cached. This will never be a name like `mainnet` or `babylonnet`.

> `GET /api/v3/chain HTTP/1.1`

```json
"NetXdQprcVkpaWU"
```

### Network parameters

Retrieve network parameter state at the time the cache was started. Note that this will have meaningless values for baking rewards/deposits.

  * Replace `NetXdQprcVkpaWU` with the chain ID from `/api/v3/chain`.

> `GET /api/v3/NetXdQprcVkpaWU/params HTTP/1.1`

```json
{
  "proof_of_work_nonce_size": 8,
  "nonce_length": 32,
  "max_revelations_per_block": 32,
  "max_operation_data_length": 16384,
  "preserved_cycles": 5,
  "blocks_per_cycle": 4096,
  "blocks_per_commitment": 32,
  "blocks_per_roll_snapshot": 256,
  "blocks_per_voting_period": 32768,
  "time_between_blocks": [
    "60",
    "75"
  ],
  "endorsers_per_block": 32,
  "hard_gas_limit_per_operation": "400000",
  "hard_gas_limit_per_block": "4000000",
  "proof_of_work_threshold": "70368744177663",
  "tokens_per_roll": 10000000000,
  "michelson_maximum_type_size": 1000,
  "seed_nonce_revelation_tip": 125000,
  "origination_burn": 257000,
  "block_security_deposit": 176000000,
  "endorsement_security_deposit": 22000000,
  "block_reward": 16000000,
  "endorsement_reward": 2000000,
  "cost_per_byte": 1000,
  "hard_storage_limit_per_operation": "60000"
}
```

### Latest (head) block

Retrieve the latest (a.k.a. head) block of all nodes being monitored.

  * Replace `NetXdQprcVkpaWU` with the chain ID from `/api/v3/chain`.

> `GET /api/v3/NetXdQprcVkpaWU/head HTTP/1.1`

```json
{
  "hash": "BLZv4orW3TXDeZPdgnaKLJCLAmXWGBEbuNP5Yy4mWFCK4nnNnZv",
  "predecessor": "BLywLvqEyTZfNZD2EnvHT4byKdBTZvuF1Ts94LueG3YB31BcCmV",
  "fitness": [
    "00",
    "000000000026db30"
  ],
  "level": 92670,
  "timestamp": "2018-09-06T23:41:57Z"
}
```

### Common ancestor

Retrieve block data for the most recent common ancestor of two blocks. This can be useful for exploring branches. For technical reasons, some included data may not be accurate (in particular, fitness and timestamp will not be known for blocks older than the time the cache started).

  * Replace `NetXdQprcVkpaWU` with the chain ID from `/api/v3/chain`.
  * Replace `BLZv4orW3TXDeZPdgnaKLJCLAmXWGBEbuNP5Yy4mWFCK4nnNnZv` with a block hash.
  * Replace `BMYJmRePKt8Y6KizS65fC89hkSFhp25jQbP2JXv1V4FnALkNpJE` with a block hash.

> `GET /api/v3/NetXdQprcVkpaWU/lca?block=BLZv4orW3TXDeZPdgnaKLJCLAmXWGBEbuNP5Yy4mWFCK4nnNnZv&block=BMYJmRePKt8Y6KizS65fC89hkSFhp25jQbP2JXv1V4FnALkNpJE HTTP/1.1`

```json
{
  "hash": "BMYJmRePKt8Y6KizS65fC89hkSFhp25jQbP2JXv1V4FnALkNpJE",
  "predecessor": "BKjF3uGPVwEQ3nVctLmNR7zLXoVoJx42yo1SyQxcrTfPx2ZbMN3",
  "fitness": [],
  "level": 0,
  "timestamp": "0000-01-01T00:00:00Z"
}
```

### Range of ancestors

Retrieve a list of ordered block hashes from a starting block and some number of levels backward.

  * Replace `NetXdQprcVkpaWU` with the chain ID from `/api/v3/chain`.
  * Replace `BLZv4orW3TXDeZPdgnaKLJCLAmXWGBEbuNP5Yy4mWFCK4nnNnZv` with some block hash.
  * Replace `10` with the number of levels backward from the given block.

> `GET /api/v3/NetXdQprcVkpaWU/ancestors?branch=BLZv4orW3TXDeZPdgnaKLJCLAmXWGBEbuNP5Yy4mWFCK4nnNnZv&level=10 HTTP/1.1`

```json
[
  "BLZv4orW3TXDeZPdgnaKLJCLAmXWGBEbuNP5Yy4mWFCK4nnNnZv",
  "BLywLvqEyTZfNZD2EnvHT4byKdBTZvuF1Ts94LueG3YB31BcCmV",
  "BMXobU8cV6ABPwW3RYbW32aZZrk44eHpV4LL87ABmBQQnYKsTr8",
  "BLqA6MUuato9xNDUiHAoAZXSxvE3GH6LYLJZsjrNnLn9SvdZv3W",
  "BLqMknvDRxXRQ7UWjvyjtLeaFuD7Q2fCarpg48RovZrxgoQxpgY",
  "BMFX9LxqY3nqMxYVWpWpbr69NMKoQdS114wG8EsEuEuWyxxnDYx",
  "BMNyZNDi4qRtJqvKAjQfGUMpVgLaPENXXtuTVS4mV4LSQB2bWLD",
  "BLJZ2M7SsQveX9htj1YJpwmf3ExgFGt7wrNxuKTtYhHFLpU1Q9p",
  "BKzTJnFdgd7imZJ72nGpPh665Fn6tvATcmdKbnrqqiw6g2Vk4BN",
  "BLVMAztP4cxsjqHZUYghuuDKgMxD8PJK1sRfoRyLF86jiKD7AAg"
]
```

### Baking rights

Retrieve the all baking rights at a specific level as seen by a given branch.

  * Replace `NetXdQprcVkpaWU` with the chain ID from `/api/v3/chain`.
  * Replace `BLVMcy2gt5Znt2f1j4AmiygsK3aRPEADPR1De5DHnpcmRb4Gq4b` with a block hash.
  * Replace `151128` with the level for desired baking rights data.

> `GET /api/v3/NetXdQprcVkpaWU/baking-rights?branch=BLVMcy2gt5Znt2f1j4AmiygsK3aRPEADPR1De5DHnpcmRb4Gq4b&level=151128 HTTP/1.1`

```json
[
  {
    "level": 151128,
    "delegate": "tz1iDu3tHhf7H4jyXk6rGV4FNUsMqQmRkwLp",
    "priority": 0,
    "estimated_time": "2018-10-18T07:37:25Z"
  },
  {
    "level": 151128,
    "delegate": "tz1S1Aew75hMrPUymqenKfHo8FspppXKpW7h",
    "priority": 1,
    "estimated_time": "2018-10-18T07:38:40Z"
  },
  {
    "level": 151128,
    "delegate": "tz1Sx1yc4cvLkcsUXD8ramcbVvj8g36y1Ms5",
    "priority": 2,
    "estimated_time": "2018-10-18T07:39:55Z"
  },
  {
    "level": 151128,
    "delegate": "tz1Lhf4J9Qxoe3DZ2nfe8FGDnvVj7oKjnMY6",
    "priority": 3,
    "estimated_time": "2018-10-18T07:41:10Z"
  }
]
```

### Endorsing rights

Retrieve the all endorsing rights at a specific level as seen by a given branch.

  * Replace `NetXdQprcVkpaWU` with the chain ID from `/api/v3/chain`.
  * Replace `BLVMcy2gt5Znt2f1j4AmiygsK3aRPEADPR1De5DHnpcmRb4Gq4b` with a block hash.
  * Replace `151128` with the level for desired endorsing rights data.

> `GET /api/v3/NetXdQprcVkpaWU/endorsing-rights?branch=BLVMcy2gt5Znt2f1j4AmiygsK3aRPEADPR1De5DHnpcmRb4Gq4b&level=151128 HTTP/1.1`

```json
[
  {
    "level": 151128,
    "delegate": "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5",
    "slots": [
      18,
      16,
      9,
      4
    ],
    "estimated_time": "2018-10-18T07:36:25Z"
  },
  {
    "level": 151128,
    "delegate": "tz3bTdwZinP8U1JmSweNzVKhmwafqWmFWRfk",
    "slots": [
      25
    ],
    "estimated_time": "2018-10-18T07:36:25Z"
  },
  {
    "level": 151128,
    "delegate": "tz3WMqdzXqRWXwyvj5Hp2H7QEepaUuS7vd9K",
    "slots": [
      30,
      0
    ],
    "estimated_time": "2018-10-18T07:36:25Z"
  }
]
```
