This document describes the procedure to migrate baking using Kiln to baking using `octez` binaries.

### Linux

Start by finding the exact commands that Kiln uses to start the node and baker
process.

[Here](https://gitlab.com/tezos-kiln/kiln/-/raw/develop/scripts/print-octez-cmds.sh) is a script that can automate this.
After inspecting it, you can proceed to source it to your terminal and obtain the working environment of the node and baker processes.

To do this, ensure that Kiln is running, the node has started, and the
baker is set up and active. With Kiln running, open the terminal and run the following
command:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://gitlab.com/tezos-kiln/kiln/-/raw/develop/scripts/print-octez-cmds.sh | CMD_NAME=node bash
```

Save the output somewhere.

Now let's do the same and fetch the command to start the baker process. In a new terminal, run the following:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://gitlab.com/tezos-kiln/kiln/-/raw/develop/scripts/print-octez-cmds.sh | CMD_NAME=baker bash
```

Now you can stop Kiln and run the two commands obtained by the previous steps. Open a terminal and paste the command
from the first step to start the `node` process. Now open another terminal and run the command obtained by the
second step to start the `baker` process. Ensure that your Ledger is plugged in and running the baker app.

If you would prefer to start from scratch or avoid running the above script, you may alternatively consult the Octez [documentation](https://octez.tezos.com/docs/introduction/tezos.html) to set up a baker using Octez.

### macOS

To do this, ensure that Kiln is running, the node has started, and the baker is set up and active. First let us get the process id and environment
of the `octez-node` process.

Open a terminal and run

```bash
ps axww -o pid= -o command= | grep '[t]ezos-node\|[o]ctez-node'
```

This should print out the process id and command line arguments that were used when starting the `octez-node` instance managed by Kiln.
Take a note of the process id (first column) and command line arguments.

Now we can do the same for `octez-baker` run the following in a terminal.

```bash
ps axww -o pid= -o command= | grep '[t]ezos-baker\|[o]ctez-baker'
```

Note down the process id and command line arguments somewhere.

Run the following command, replacing `<pid-of-node>` with the process id of the `octez-node` process, obtained in the steps above to
get the working directory of the running node process(this is often `/Users/<username>/Library/Kiln`).

```bash
lsof -a -p <pid-of-node> -d cwd
```

Note that if you wish to keep using the Octez binaries that shipped with Kiln, you might have to include the library path that contains the shared libraries by using `DYLD_FALLBACK_LIBRARY_PATH` environment variable before launching the executables.

To get the expected value for this environment variable, run the following.

```bash
ps eww -p <pid-of-node>
```

In the output look for the `DYLD_FALLBACK_LIBRARY_PATH` and the value assigned to it. This is typically `/usr/local/kiln-nix/lib`.
Use it to set the environment variable as shown below.

```bash
export DYLD_FALLBACK_LIBRARY_PATH=/usr/local/kiln-nix/lib
```

Once you get the working directory and optionally set the environment variable, do the following:

1. Stop the Kiln process using `launchctl stop tezos.kiln`.
2. Open a new terminal and `cd` to the working directory we obtained earlier.
3. Start the standalone `octez-node` using the command line arguments we obtained from the `octez-node` process that was
managed by Kiln.
4. Open another terminal and do the same for `octez-baker` process, but using the command line arguments we obtained for the baker.

Now you have successfully migrated your Tezos baking from Kiln to Octez binaries.

Not that here as well, if you would prefer to start from scratch, you may
alternatively consult the Octez
[documentation](https://octez.tezos.com/docs/introduction/tezos.html) to set up
a baker using Octez.

