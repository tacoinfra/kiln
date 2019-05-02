# Virtual Machine

Obsidian Systems provides Kiln virtual machine releases as a `.ova` file, which can be found at https://gitlab.com/obsidian.systems/tezos-bake-monitor/releases beginning with `v0.5.2`.
The VM has been tested on VirtualBox only, on the host OS: Windows, Mac and Ubuntu.
But this might work with other VM software like VMWare, and other hosts.

## Installation

- Install [VirtualBox][1] for your system
- Download the `kiln-vm.ova` file from the Kiln release page, and import this in VirtualBox from the “File” -> "Import Appliance" option.

    - During import step you can configure the number of processors and memory for the VM
    - It is recommended to increase the processor value to at least 2.

## Running

- After successful import, run the "Kiln VM" by clicking the "Start"/ "Normal Start" button on menu

- Once the VM is running, Kiln will run automatically. Open http://localhost:8000 from the browser (FireFox) in the VM to access Kiln.

## Configuration

Kiln can be configured by putting a config file with the argument contents into a file in the `/home/kiln/app/config` directory.
The details of configuration options is available in the [config docs][2]

After adding the configuration use command `sudo systemctl restart kiln` to restart kiln with new configuration.

Note:
- The VM user ‘kiln’ has ‘sudo’ access and has no password.
- To open terminal in VM: Click KDE ‘start’ Menu -> Applications -> System -> Terminal

For example to run zeronet

```
mkdir -p /home/kiln/app/config
echo “zeronet” > /home/kiln/app/config/network
sudo systemctl restart kiln
```

## Upgrade Kiln

Use command `upgrade-kiln` from terminal to upgrade kiln to latest version.

## Using Ledger device with VirtualBox

In order to do baking with Kiln you need to enable the Ledger device from its USB settings.

- Connect the Ledger device and enter the passcode.

- Open the USB settings for "Kiln VM" by going to the "Devices" -> "USB" -> "USB settings" menu.

- In the USB settings, click the button with a ‘+’ mark ('Add new USB filter with all fields set to values of the selected USB device'), and select the “Ledger Nano S” device

 
- After enabling this reconnect the Ledger device, and enter the passcode again. Then restart the VM.

- After restarting VM you can check if the device is detected properly by the `tezos-client list connected ledgers` command.

## Using tezos binaries

`tezos-client`, `tezos-admin-client` and other `tezos-*` binaries are available to use from the terminal.

## Moving chain data to separate disk

The VM uses two virtual disks; one for the operating system, and another to store the Kiln data (including the chain data).
The capacity of primary disk is 64 gb out of which about 8gb is used by the operating system.
The capacity of data disk is 500gb and it stores all the chain data. The data disk is mounted on `/home/kiln/app` directory

Since it is a separate file it can be conviniently copied in case your disk becomes full, or you want to get the chain data from another location.

To change the location of the disk, first turn off the VM, then go to "File" -> "Virtual Media Manager"

In the "HDD" tab right click "kiln-vm-disk002.vmdk" and select "Move..."
Specify the target location in the next dialog.


[1]: https://www.virtualbox.org/wiki/Downloads
[2]: https://gitlab.com/obsidian.systems/tezos-bake-monitor/blob/develop/docs/config.md
