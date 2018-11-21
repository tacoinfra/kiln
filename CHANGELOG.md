# Changelog

## 0.2.1 (November 21, 2018)

  * **Major bug fix:** Older versions of Kiln excercise a Tezos node bug that was fixed in https://gitlab.com/tezos/tezos/merge_requests/705. If your node does not include this patch Kiln will fail to sync with your node. Some public nodes may still exhibit this bug at the time of release.
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
