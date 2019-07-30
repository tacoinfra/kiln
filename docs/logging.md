# Logging

## Quick start

Create a file called `loggers` in the `config` dir adjacent to the `backend` executable.

```json
[
  { "logger":{"Stderr":{}}
  , "filters":
      { "SQL":"Error"
      , "":"Debug"
      }
  } ,
  { "logger":{"File":{"file":"sql.log"}}
  , "filters":
      { "SQL":"Debug"
      }
  } ,
  { "logger":{"File":{"file":"kiln-node.log"}}
  , "filters":
      { "kiln-node":"Info"
      }
  } ,
  { "logger": {"Journald":{"syslogIdentifier":"kiln"}}
  , "filters":
      { "":"Warn"
      }
  }
]
```

With this config all logging output will go to `stderr`, except raw sql queries.
Sql queries will be logged to a file named `sql.log`.
Kiln node's output will go to `kiln-node.log`,
and all errors and warnings will also be logged to `Journal`.


## Loggers configuration details

The `config/loggers` is a json document having a list of objects with two properties,

### `logger`

The `logger` property defines the output destination.
The below three loggers are available.  They're encoded as a 1 property object with the
output type as its name and all options as an object.

- `Stderr` logs to the console.  There are no options

- `File` logs to a file.

  It has one option for file name `"file"`.
  The file logger does no automatic file rotation, but works in
  append mode, so should be compatible with normal devops tools like `logrotate`.

- `Journald` logs to the systemd journal.

  It has one option, `syslogIdentifier`
  which will be used as the identifier in the generated journald record.
  
  This identifier can be used in the `journalctl` command's `--identifier` option.

### `filters`

An optional property of the logger object, `"filters"` controls which log messages are logged.

If absent or empty, then by default all messages are discarded.

This property should contain a map from log 'category' to log 'level'.
The 'category' of a message is the prefix of the message like `SQL`, `kiln-node`, `kiln-baker`, `kiln-endorser`, etc.
When the category is specifed, then the messages with a priority at least as high as the 'level' value will be logged.
All other messages will be discarded.

If the 'category' is empty string `""`, then all messages of that 'level' and higher will be logged.

Log levels are `Error`, `Warn`, `Info`, and `Debug`.
