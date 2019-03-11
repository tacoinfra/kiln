# Accused of double baking and Accused of double endorsement alerts

## Intent

These alerts should appear whenever an accusation presenting evidence of double baking (resp. endorsing) by a monitored baker appears on any branch of the Tezos network, unless it is too old to appear again on a future head branch.

## Tests

  1. Generate or obtain a docker image of the Kiln version being tested.
  1. Load or pull the docker image and remember its identifier.
  1. Check out an appropriate version of `tezos-baking-platform`, currently `jd-flextesa-accusations`.
  1. Run `scripts/run-accusation-tests.sh <docker-image-identifier>`.
  1. When the script pauses and asks you to configure bakers in Kiln, follow the instructions.
  1. Additionally configure the monitor to send alerts via email or some other medium.
  1. Enter `q` to continue the test script.
  1. Wait for a few minutes for the test script to finish the scenario and pause.
  1. Verify that you see and receive a notification of a double baking accusation.  Notifications for missed bakes and endorses may also occur.
  1. Enter `q` to proceed to the double endorsement scenario.
  1. When the script pauses and asks you to configure bakers in Kiln, reload or reopen the Kiln page to clear state from the prior scenario.
  1. Then follow the rest of the instructions as given.
  1. Additionally configure the monitor to send alerts via email or some other medium.
  1. Enter `q` to continue the test script.
  1. Wait for a few minutes for the test script to finish the scenario and pause.
  1. Verify that you see and receive a notification of a double endorsement accusation.  Notifications for missed bakes and endorses may also occur.
  1. Enter `q` to shut down the testing environment.
