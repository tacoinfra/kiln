# Changelog

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
