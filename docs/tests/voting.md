# Voting with Kiln Baker

## Intent

Users should be able to vote from within Kiln using a Kiln Baker. This should work for all voting periods.

This scenario can also be used to check whether Kiln displays amendment notifications correctly.

Note, that some steps of this scenario are time sensitive due to the fact that Kiln
doesn't handle blocks whose timestamp is older than 10 minutes from the current time.

## Test scenario workflow

  1. Install Kiln to the `app` folder:
     ```shell
     # This command should be run from the root of the repository
     $(nix-build --no-out-link -A installKiln --argstr closure-compiler-setting "BUNDLE")/bin/install-kiln app
     ```
  2. Plug in a Ledger and open the Tezos Baking app.
  3. In a separate terminal start a [voting scenario script](https://gitlab.com/morley-framework/local-chain#voting-scenario) from the local-chain repo.
     This script will provide you with a path to the node config that will be used to start a Kiln node.
  4. Start Kiln:
     ```shell
     (cd app && ./backend --node-config-file <path to the node config> --rights-history-window=10)
     ```
  5. Add a Kiln Node and wait for it to sync using 'Peer to Peer Download' option for bootstrap.
  6. Choose a 'tz' address stored on your ledger and provide it to the `voting.py` script.
     This address will receive some amount of XTZ.
  7. Add a Kiln baker using the previously chosen address.
  8. After that `voting.py` will start going through the voting cycle.
  9. The script will stop at the beginning of each voting period that requires voting and ask you to vote.
      Open the Tezos Wallet app on your Ledger and vote using Kiln on each period. Once you'll vote, you should
      prompt the script to continue going through the voting cycle.

To clean up and start everything from scratch run:
```shell
rm -rf app/db app/.kiln
```
