# Ubuntu Distribution

Obsidian Systems packages Kiln releases as a .deb file, which can be found at https://gitlab.com/obsidian.systems/tezos-bake-monitor/releases beginning with v0.5.1. This package has been tested on Ubuntu only, but we plan on supporting other linux distributions in the near future.

To get started:

1. Download the deb file from our [releases](https://gitlab.com/obsidian.systems/tezos-bake-monitor/releases) page.
2. Open the file.
3. Follow the installation instructions.
4. Open [http://localhost:8000](http://localhost:8000)

## Configuring the Ubuntu Package

By default, the ubuntu installation runs on mainnet with a standard options, like using port `8000`. You can use Kiln on a test network and configure advanced settings in its config file, located at `/etc/kiln`.

To change the port, network, or specify other arguments, add the relevant options in file `/etc/kiln/args`. For example: 

```
KILNARGS="--network=zeronet -- --port=8080"
```

This sets Kiln to run on zeronet and use the port 8080. Note the `--` before `--port` configuration. After editing the file, do `sudo systemctl restart kiln` to start kiln with the new configuration.

## Accessing Logs

Kiln's logs can be found in journal control. Some useful commands for viewing logs include:

* `journalctl -u kiln` - View all of Kiln’s logs since inception
* `journalctl -u kiln -S YYYY-MM-DD` - View Kiln's logs since the date provided
* `journalctl -u kiln -S "YYYY-MM-DD HH:MM"` - View Kiln's logs since the date and time provided
* `journalctl -u kiln -ef` - View and follow Kiln's most recent logs
* `Shift + G` - Jump to the most recent logs
* `Ctrl + C` - Exit journal control

## Running tezos-client and tezos-admin-client

Use the command `kiln-shell` to create a shell from where you can use the `tezos-client` and `tezos-admin-client` for mainnet.
For other networks specify the network like `kiln-shell alphanet` or `kiln-shell zeronet`.

Note: This shell uses a temporary directory `/tmp/kiln-shell-home` to store the data created by `tezos-client`.
