# Building Kiln from Source

## Prerequisites

These builds have only been tested on Linux.

### Configuring the Nix cache (Recommended)

If you have not done so already, we recommend you add our Nix caches to your Nix configuration to drastically reduce your build time. Please see instructions in [Tezos Baking Platform](https://gitlab.com/obsidian.systems/tezos-baking-platform/blob/develop/README.md#setting-up-nix-caching-recommended).

### Cloning the repository

```shell
git clone https://gitlab.com/obsidian.systems/tezos-bake-monitor.git
cd tezos-bake-monitor/
```

By default you will be on the `develop` branch which is the latest unstable version. For a stable version, checkout `master` or one of the specific version tags.

### Running the build and installing Kiln

Run this command to link the build’s path to the 'app' directory.

```shell
$(nix-build -A installKiln --no-out-link)/bin/install-kiln
```

You can also specify a custom directory as an argument to this command

```shell
$(nix-build -A installKiln --no-out-link)/bin/install-kiln kiln-latest
```

### Starting the monitor

To run the monitor, enter the `app` directory and start the `backend`. Replace `<network>` with your desired Tezos network, e.g. `zeronet`, `alphanet`, `mainnet`, or with a specific chain ID.

```shell
cd app
./backend --network <network>
```

If you completed these steps correctly, your Monitor should now be running at http://127.0.0.1:8000.

### Command Line Options

For the most complete list of options while starting Kiln, run `./backend --help`. Some of the most helpful options are:

* `--network <network>` - Values are 'zeronet', 'alphanet', and 'mainnet' (default).
* `--node` or `--nodes` - Adds monitored nodes to Kiln at startup. Entries should be comma separated, and can optionally take an alias.
    *   Note: Nodes must begin with `http://`
    *   Example: `--nodes http://localhost:8732,http://localhost:8733@SecondNode`
* `--baker` or `--bakers` - Adds monitored bakers to Kiln at startup. Entries should be comma separated, and can optionally take an alias.
    *   Example: `--bakers tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9@FirstBaker,tz3NExpXn9aPNZPorRE4SdjJ2RGrfbJgMAaV@SecondBaker,tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r`
* `--kiln-node-rpc-port=PORT` - Configures the RPC port of the Kiln Node. Default is `8733`
* `--kiln-node-net-port=PORT` - Configures the Net port of the Kiln Node. Default is `9733`

### Updating from an older source build

To update your source build, simply checkout the newer version and `git pull`. For example:

```shell
git checkout master
git pull
```

Then follow the steps in [Running the build](#running-the-build). However, you'll already have an `app` directory. Deleting it would remove your database as well since your database is stored in `app/db`. You can simply overwrite the necessary application files by rerunning the `install-kiln` command.

```shell
$(nix-build -A installKiln --no-out-link)/bin/install-kiln
```
