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
