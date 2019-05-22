# Voting with Kiln Baker

## Intent

Users should be able to vote from within Kiln using a Kiln Baker. This should work for all voting periods.

## Tests

**NOTE:** All commands must be run from the root of this repository.

  1. Install Kiln to the `app` folder:
     ```shell
     $(nix-shell --no-out-link -A installKiln)/bin/install-kiln app
     ```
  2. Plug in a Ledger and open Tezos Baking.
  3. In a separate terminal start the protocol-transition test and tell it to install Kiln configs in `app`.
     ```shell
     $(nix-shell --no-out-link -A tests.protocol)/bin/protocol-test app
     ```
  4. Wait for the `protocol-test` script to get to a point where it says the following (it may take a minute or so):
     ```
     Flextesa.daemons-upgrade:
       Pause
         Test becomes interactive.
         Kiln was configured at `app/config`
         Please type `q` to start a voting/protocol-change period.

     Flextesa.daemons-upgrade: Please enter command:
     ```
  5. Start Kiln (in a new shell):
     ```shell
     cd app && ./backend
     ```
  6. Add a Kiln Node and wait for it to sync.
  7. Add a Kiln baker and use the account that has a lot of tez.
  8. In the shell where `protocol-test` is waiting for your input, enter `q` and hit Enter. This will start the protocol transition. If the test dies without transitioning to the next period, start it again (step 3) and hit `q` and again.
  9. Go forth and vote on all the voting periods. You don't have lots of time to do it.

To clean up and start everything from scratch run:

```shell
rm -rf app dpu_root_path
```

You may also need to run `pkill tezos-node` to stop old nodes.
