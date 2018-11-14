# Changelog

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
