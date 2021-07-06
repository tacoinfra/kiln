# Automatic Upgrade Check

## Intent

Unless the setting is disabled, the monitor should automically check for newer versions of itself and tell the user.

## Tests

### Testing when there is a new version
  1. Run the monitor like this: `./backend --upgrade-branch=TEST-UPGRADE`
  2. Open the monitor in the browser and you should immediately see a banner saying that there is a new version.
  3. In the Options tab, click "Check for New Version".
  4. It should tell you there is a new version.

### Testing when no new version
  1. Run the monitor like this: `./backend --upgrade-branch=develop`
  2. Open the monitor in the browser and there should be not be a banner mentioning upgrades.
  3. In the Options tab, click "Check for New Version".
  4. It should tell you that you're using the latest version.

### Testing when the version check fails
  1. Run the monitor like this: `./backend --upgrade-branch=not-a-real-branch`.
  2. Open the monitor in the browser and you should immediately see a banner saying that the version check failed.
  3. In the Options tab, click "Check for New Version".
  4. It should tell you that the check failed.

### Testing that the version check can be disabled
  1. Run the monitor like this: `./backend --check-for-upgrade=no`.
  2. Open the monitor in the browser and there should be not be a banner mentioning upgrades.
  3. In the Options tab, the "Check for New Version" but should be missing.

### Testing new tezos-core versions:
  1. Fork Tezos repo in gitlab ***DO NOT USE PUSH CHANGES TO*** `gitlab.org/obsidian.systems/tezos`

      1. go to https://gitlab.com/obsidian.systems/tezos
      1. click `fork` link at the top right.
      1. choose yourself.
      1. locate the project ID for the new fork;  it may be displayed under the `tezos` title in the top center of the page

  1. Use the command line argument `--network-gitlab-project-id` to reference
     your forked version of Tezos.
     `./backend --network-gitlab-project-id=PROJECTID [OTHER OPTIONS]`

      1. Alternatively, you should be able to place the project ID in this file, but that did not work for me - https://gitlab.com/obsidian.systems/kiln/blob/f34428af76ed11add0c973facffe392f0e283f05/tezos-bake-central/config/network-gitlab-project-id
        this is the only way to set this option in `ob run`, but our default instructions make the `config/app` dir non-writeable.

  1. Run Kiln with that project ID so it establishes a HEAD commit
  1. Stop running Kiln
  1. Go to the branch you are monitoring with Kiln (babylonnet for me) and make a simple commit. Here's mine - https://gitlab.com/mikereinhart/tezos/commit/fad61ab65ecc30a20b2df629c28274bd45b57076
  1. Restart Kiln. It checks for an update at startup or every hour. You should
     now see the notification.  use the `--network-gitlab-project-id=PROJECTID`
