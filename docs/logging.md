## Logging

# Quick start

create a file called `loggers` in the `config` dir adjacent to the `backend` exceutable.

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
      { "":"Error"
      , "SQL":"Debug"
      }
  } ,
  { "logger": {"Journald":{"syslogIdentifier":"kiln"}}
  }
]
```

all logging output will go to stderr, except sql queries.  sql queries will be
logged to a file named `sql.log` and all errors and warnings will also be
logged to JournalD


# Loggers

the structure of the `config/loggers` is a list of objects with two properties,
the `logger` property defines the output destination.  At this writing, three
loggers are implemented.  They're encoded as a 1 property object with the
output type as its name and all options as an object.

`Stderr` logs to the console.  there are no options

`File` logs to a file.  it has one option, `"file", which names the to be
logged to.  The file logger does no automatic file rotation, but works in
append mode, so should be compatible with normal devops tools like
`logrotated`.

`Journald` logs to the systemd journal.  it has one option, `syslogIdentifier`
which will be used as the identifier in the generated journald record.

# Filtering

an optional property on the logger object, `"filters"`. which controls which
log messages are written.  if absent or empty, then by default, all messages at
level `Warn` or `Error` are logged, and all others are discarded, the property
should contain a map from log category to level.  messages with a prefix of the
given category, with a priority at least as high as the value will be logged.
all others discarded.

Log levels are `Error`, `Warn`, `Info`, and `Debug`.
