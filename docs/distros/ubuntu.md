# Ubuntu Distribution

Obsidian Systems packages Kiln releases as a .deb file, which can be found at https://gitlab.com/obsidian.systems/kiln/-/releases beginning with v0.5.1. This package has been tested on Ubuntu only, but we plan on supporting other linux distributions in the near future.

To get started:

1. Download the deb file from our [releases](https://gitlab.com/obsidian.systems/kiln/-/releases) page.
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

## Running tezos-client and tezos-admin-client binaries

Use the command `kiln-shell` to create a shell from where you can use the `tezos-client` and `tezos-admin-client` for mainnet.
For other networks specify the network like `kiln-shell babylonnet` or `kiln-shell zeronet`.

Note: when running `tezos-client` you might get this message "umount: /oldroot: filesystem was unmounted, but failed to update userspace mount table.", but this can be ignored.

```
baker@ubuntu:~$ kiln-shell
Starting kiln-shell for mainnet.
To run kiln-shell for other network, please specify 'kiln-shell babylonnet' or 'kiln-shell zeronet'.
baker@ubuntu:~$ tezos-client -P 8733 list connected ledgers
umount: /oldroot: filesystem was unmounted, but failed to update userspace mount table.
Disclaimer:
  The  Tezos  network  is  a  new  blockchain technology.
  Users are  solely responsible  for any risks associated
  with usage of the Tezos network.  Users should do their
  own  research to determine  if Tezos is the appropriate
  platform for their needs and should apply judgement and
  care in their network interactions.

Found a Tezos Baking 2.0.0 (commit v1.5.0-120-g74da3e65) application running on Ledger Nano S at [0001:0010:00].

To use keys at BIP32 path m/44'/1729'/0'/0' (default Tezos key path), use one of
 tezos-client import secret key ledger_divam "ledger://frilly-elephant-alienated-hippopotamus/ed25519/0'/0'"
 tezos-client import secret key ledger_divam "ledger://frilly-elephant-alienated-hippopotamus/secp256k1/0'/0'"
 tezos-client import secret key ledger_divam "ledger://frilly-elephant-alienated-hippopotamus/P-256/0'/0'"
```

To use the binaries for a different network please specify 'kiln-shell babylonnet' or 'kiln-shell zeronet'

## Uninstalling Kiln

Kiln can be uninstalled using the following command. Note this will remove all the configurations and node data.

```
sudo dpkg --purge kiln
```
