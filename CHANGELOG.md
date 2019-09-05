# Changelog

## 0.6.2

  * Update mainnet tezos binaries. This fixes the problem of 100% cpu usage by the tezos-node (and "updater: COMPILATION ERROR" messages). The tezos-node released with v0.6.1 has problem in compiling the new protocol binaries (PsBABY5HQTSk), and this issue surfaced now because mainnet entered the testing phase of the protocol amendement cycle.

## 0.6.1

  * Export logs for Kiln node, baker, and endorser via UI on Ubuntu/Debian.
  * Fix 'Add Baker' flow issues when communicating with tezos-client.
  * Performance improvements and bug fixes for alerts.
  * Fix a bug where already registered baker could not be added.
  * Bug fixes for UI.

## 0.6.0

  * Support for monitoring nodes running in *full* history mode. (Note that monitoring nodes running in *rolling* history mode is still not officially supported.)
  * Support for starting the Kiln Node in *full* mode from a snapshot. If you upgrade from a previous version of Kiln where you've already started a Kiln Node, that node will remain in *archive* history mode. If you delete that node and create a new Kiln Node, it will run in *full* history mode and allow you to start from a snapshot. All newly created Kiln Nodes will run in *full* history mode.
  * The Obsidian Public Node is now enabled by default. You can disable it with a command-line option. However, if you do this, you must monitor at least one node in *archive* history mode for Kiln to work correctly. This is because Kiln needs access to at least one source of network history that can go back to the beginning.
  * New voting reminder alerts.
  * A new alert for Kiln upgrades.
  * Bug fixes and UI improvements.


## 0.5.3
*June 11, 2019*

  * The Kiln baker tile now shows the status of your vote (not yet voted, pending, and included).
  * When setting up the Kiln baker, Kiln now checks to see if the baker address has already registered and skips those steps if possible.
  * Small UI improvements
  * Bug fixes

### Known Issues

  * This version of Kiln **does not support** the new "Full" and "Rolling" history modes of Tezos nodes. The Kiln node always uses "Archive" mode for now and if you monitor a non-Archive node, Kiln may not be able to collect all information. We are actively working to add support for this.
  * This version of Kiln **does not support** the most recent version of zeronet as of the release date (chain ID `NetXkaRXbyeogSM`).

## 0.5.2
*May 24, 2019*

  * Voting support for Kiln bakers!
  * New governance status popup showing current votes, proposals, etc.
  * Kiln baking will seamlessly transition between protocol 003 and 004.
  * Debian package installer now works on *Debian* (not just Ubuntu)!
  * Debian installer now automatically configures all udev rules necessary for ledger communication.
  * Kiln's node now uses non-default ports: 8733 for RPC and 9733 for P2P. This was done to avoid
    conflicting with nodes run outside of Kiln. These ports be changed via Kiln configuration.
  * Minor improvements:
      * Input validation on ledger BIP32 paths
      * Improved documentation
      * Updated Kiln logo
      * New public-facing Nix cache for Kiln and all tezos executables (`tezos-client`, etc.). Refer to [Setting up Nix Caching](https://gitlab.com/obsidian.systems/tezos-baking-platform#setting-up-nix-caching-recommended) for details.
  * Bug Fixes:
    * Improved accuracy surrounding data for nodes that are having connectivity issues
    * Improved correctness surrounding notifications for inactive bakers

### Known Issues

  * This version of Kiln **does not support** the most recent version of zeronet as of the release date (chain ID `NetXkaRXbyeogSM`).


## 0.5.1
*April 16, 2019*

  * New settings for missed bake/endorsement notification frequency
  * New button to resolve all notifications
  * A new "You will be deactivated" notification
  * Installation via Debian package
  * UI updates and bug fixes


## 0.5.0
*March 29, 2019*

  * Support for baking and endorsing directly in Kiln! (Requires a Ledger Nano S)
  * Support for various Ledger interactions via Kiln:
      * Authorize/Re-authorize baking
      * Set high-water mark
  * Support for Telegram notifications to private groups
  * New configuration options via command-line/config files
  * Improved performance and stability
      * Data from public nodes is updated more reliably
      * Limited number of notifications on frontend
  * Bug fixes
      * Fixed incorrect "Node is on a branch" bug from 0.4.1
      * Available balance for monitored bakers is now correct


## 0.4.1
*March 11, 2019*

  * Kiln now sends alerts if it sees a double baking or double endorsement accusation for a monitored baker.
  * Performance improvements
  * Bug fixes

### Known Issues

  * Some unresolved alerts still show up under **Resolved**.

## 0.4.0
*January 25, 2019*

  * Kiln can now launch and monitor a node internal.
  * Minimum connections alerts reported for nodes with too few peers.
  * Available balance and staking balance on Baker's Tile so that bakers can quickly see how much tez funds are available for security deposits and staking
  * Telegram/email notification for when the tezos-core is updated
  * Improved logging
  * Bug fixes, including all known issues from 0.3.0

## 0.3.0
*January 9. 2019*

  * Kiln can now monitor key stastics about baker accounts
    * Each baker's next baking or endorsing opportunity is displayed
    * Missed baking or endorsing opportunities trigger alerts
    * Bakers becoming deactivated or soon to be deactivated trigger an alert.
  * Kiln informs when new versions of tezos-core are released, as well as new versions of Kiln itself.
  * Minor UI improvements
  * Performance improvements
  * Bug fixes

### Known Issues

  * Unregistered baker addresses cause Kiln to crash.   Workaround: make sure your baker address is self delegated before adding it to Kiln.
  * Certain alerts are no longer visible once they are resolved by the user.
  * When Kiln cannot gather data about a baker because there aren't sufficient nodes, the status icon of the baker should be red but is green

## 0.2.3
*November 26, 2018*

**NOTICE: An important update to Tezos is coming on Monday, November 26. We strongly recommend you join the Baker Slack channel for updates surrounding this breaking change. Please email <tezos@obsidian.systems> to join.**

  * **Critical update:** Support for [protocol 003_PsddFKi3](https://tezos.gitlab.io/master/protocols/003_PsddFKi3.html).
  * Alerts for when your node is behind or on a branch will now only trigger after the error state has existed for 3 minutes or more. Kiln is also a bit more particular about what triggers this alert in the first place.
  * The alert panel in the web interface now only shows alerts that have been last seen in the last 36 hours. This is a temporary solution that makes the app work for users who have thousands of alerts.

## 0.2.2
*November 21, 2018*

**NOTICE: An important update to Tezos is coming on Monday, November 26. We strongly recommend you join the Baker Slack channel for updates surrounding this breaking change. Please email <tezos@obsidian.systems> to join.**

  * **Major bug fix:** Older versions of Kiln exercise a Tezos node bug that was just recently fixed in <https://gitlab.com/tezos/tezos/merge_requests/705>. If your nodes do not include this fix, Kiln will fail to sync with your nodes. Some public nodes may also exhibit this bug. This Kiln update includes a workaround that will allow Kiln to sync with any Tezos node even without the bug fix.
  * Alert resolution now triggers a notification (email or Telegram).
  * Logging for alerts to console, file, or systemd journal. Alerts are logged under the "Kiln" category.
  * Minor bug fixes

## 0.2
*November 14, 2018*

  * Telegram support for notifications
  * Completely revamped UI
  * Configurable logging (to journald, to files, to stdout/stderr, configurable filtering, levels, etc.)
  * The frontend now reports when it is not actively connected to the backend (fixed known issue from version 0.1)
  * Minor bug fixes

## 0.1
*October 9, 2018*

  * Initial release.

### Known Issues

  * If the frontend page loses connection to the server it will stop showing live data. This might happen if, for example, your computer goes to sleep with the page open. For now, you need to manually refresh the page to reconnect. This will be fixed in a future release.
