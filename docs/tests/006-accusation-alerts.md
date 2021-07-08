# Accused of double baking and Accused of double endorsement alerts

## Intent

These alerts should appear whenever an accusation presenting evidence of double baking (resp. endorsing) by a monitored baker appears on any branch of the Tezos network, unless it is too old to appear again on a future head branch.

## Tests

**NOTE:** All commands must be run from the root of this repository. If you're testing via `ob run`, use `tezos-bake-central` instead of `app`.

  1. Install Kiln to the `app` folder:
     ```shell
     $(nix-build --no-out-link -A installKiln)/bin/install-kiln app

  1. In a separate terminal start the accusations test and tell it to install Kiln configs in `app`.
     ```shell
     $(nix-build --no-out-link -A tests.accusations)/bin/accusations-test app
     ```
     Enter `q` to when the test script starts and gives a prompt.

     Note: By default this script will run the double baking accusation test. To run the double endorsing accusation test specify 'simple-double-endorsing' after the name of the directory.

     ```shell
     $(nix-build --no-out-link -A tests.accusations)/bin/accusations-test app simple-double-endorsing

  1. Wait for a few seconds after the `accusations-test` starts showing messages like this
     ```
       Ensure-protocol-default-bootstrap 1. directory
       Ensure-protocol-default-bootstrap 2. sandbox.json
       Ensure-protocol-default-bootstrap 3. protocol_parameters.json
       <DBG| Trying to bootstrap client |DBG>
       <DBG| Waiting for all nodes to be bootstrapped |DBG>
       Flextesa.accusing:
        Successful bake (C-Simple000: first bakes: [1/49]): [
          "Injected block BKye6FJuSrfs"
        ]
     ```

  1. Start Kiln: (At the moment it is necessary to remove the file `config/binary-paths` manually)
     ```shell
     (cd app && rm config/binary-paths && ./backend)
     ```
     Once the Kiln is started open http://localhost:8000

  1. Additionally configure the Kiln to send alerts via email or some other medium.

  1. After a few minutes the script should pause and display this. Enter `q` to continue the test script.
  ```
     Flextesa.accusing:
       Pause
         Clients ready Node 0 baked 49 times. All nodes should be at level 50.

     Flextesa.accusing: Please enter command:
  ```

  1. Again enter `q` to continue the test script
  ```
     Flextesa.accusing: Please enter command:
  ```

  1. Then the script should pause and display this. Enter `q` to continue the test script.

  ```
     All nodes reaching level 52

     Flextesa.accusing: Please enter command:
  ```

  1. Once the script pauses this time and displays a similar message, check Kiln for the accusation alert.
     Notifications for missed bakes and endorses may also occur.
     After confirming the alert Enter `q` to continue the test script.

  ```
     Flextesa.accusing:
       Successful bake (C-Simple002: all at lvl 52): [
         "Injected block BLJ9YasMZib1"
       ]
     Flextesa.accusing: Pause  Just baked what's the level? Vs 53
     Flextesa.accusing: Please enter command:
  ```

  1. Enter `q` to shut down the testing environment.

  ```
    Flextesa.accusing: Test done.
    Flextesa.accusing: Pause  Scenario done; pausing
    Flextesa.accusing: Please enter command:
  ```
