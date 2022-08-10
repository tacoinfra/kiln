# Logging

## Quick start

Create a file called `loggers` in the `config` directory adjacent to the `backend` executable.

```json
[
  {
    "logger": { "Stderr": {} },
    "filters": {
      "SQL": "Error",
      "": "Debug"
    }
  },
  {
    "logger": { "File": { "file":"sql.log" } },
    "filters": { "SQL": "Debug" },
  },
  {
    "logger": { "File": { "file": "kiln-node.log" } },
    "filters": { "kiln-node": "Info" }
  },
  {
    "logger": { "Journald": { "syslogIdentifier": "kiln" } },
    "filters": { "": "Warn" }
  }
]
```

With this configuration all logging output will go to `stderr`, except raw SQL queries. SQL queries will be logged to a file named `sql.log`. The Kiln Node's output will go to `kiln-node.log`, and all errors and warnings will also be logged to `Journal`.


## Loggers configuration details

The `config/loggers` file is a JSON document with a list of objects each with two properties:

### `logger`

The `logger` property defines the output destination. Three loggers are available. They're encoded as a 1 property object with the output type as its name and all options as an object.

  * `Stderr` logs to the console. It has no options so you must use `{}` as the value.
  * `File` logs to a file. This has one option `"file"`, for the path to an output file. The file logger does not do automatic file rotation but runs in append mode so should be compatible with normal devops tools like `logrotate`.
  * `Journald` logs to the systemd journal. This has has one option, `syslogIdentifier`, which will be used as the identifier in the generated journald record. This identifier can be used in the `journalctl` command's `--identifier` option.

### `filters`

`filters` is an optional property of the logger object and controls which log messages are logged. If absent or empty all messages are discarded.

This property should contain a map from log 'category' to log 'level'. The 'category' of a message is the prefix of the message like `SQL`, `kiln-node`, `kiln-baker`, etc. When the category is specifed, the corresponding messages with a level at least as high as the 'level' value will be logged. All other messages will be discarded. If the 'category' is the empty string `""`, then all messages of that 'level' and higher will be logged. Log levels are `Error`, `Warn`, `Info`, and `Debug`.
