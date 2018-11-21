# Changelog

## 0.2.2 (November 21, 2018)

**NOTICE: An important update to Tezos is coming on Monday, November 26. We strongly recommend you join the Baker Slack channel for updates surrounding this breaking change. Please email <tezos@obsidian.systems> to join.**

  * **Major bug fix:** Older versions of Kiln exercise a Tezos node bug that was just recently fixed in <https://gitlab.com/tezos/tezos/merge_requests/705>. If your nodes do not include this fix, Kiln will fail to sync with your nodes. Some public nodes may also exhibit this bug. This Kiln update includes a workaround that will allow Kiln to sync with any Tezos node even without the bug fix.
  * Alert resolution now triggers a notification (email or Telegram).
  * Logging for alerts to console, file, or systemd journal. Alerts are logged under the "Kiln" category.
  * Minor bug fixes

## 0.2 (November 14, 2018)

  * Telegram support for notifications
  * Completely revamped UI
  * Configurable logging (to journald, to files, to stdout/stderr, configurable filtering, levels, etc.)
  * The frontend now reports when it is not actively connected to the backend (fixed known issue from version 0.1)
  * Minor bug fixes

## 0.1 (October 9, 2018)

  * Initial release.

### Known Issues

  * If the frontend page loses connection to the server it will stop showing live data. This might happen if, for example, your computer goes to sleep with the page open. For now, you need to manually refresh the page to reconnect. This will be fixed in a future release.
