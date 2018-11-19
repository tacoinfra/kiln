# Tezos Bake Monitor

## Building from Source

  1. Install the [Nix](https://nixos.org/nix/) package manager.
  2. In `tezos-bake-central` run `nix-build -A exe --out-link result`. This will build the project and create a symlink in the current directory called `result`.
  3. `result/backend` is the web server for the application. Use `result/backend --help` to see some options.
  4. When running the application, use `cd result && ./backend [OPTIONS]` so `backend` can find assets and configurations.

Note: Builds have only been tested on macOS and Linux. Windows might work if you use WSL, but it has not been tested.

### Setting up the database

#### Using a self-managed database

By default the bake monitor will create and manage its own Postgres database. To use this behavior, the `result` directory structure must exist where it has write permissions. Since the build result is in a read-only nix folder, you need copy or symlink the `result` somewhere else:

```shell
mkdir app
ln -s $(realpath result)/* app/
cd app
./backend # This will create a new `db` folder where the Postgres database files live
```

(You may also create a sibling file called `db` with a connection string in it. In this case, `backend` will use the connection string instead.)

#### Connecting to an external database

Alternatively, you can tell the `backend` to connect to another Postgres database with the `--pg-connection=CONNSTRING` option. You can use any connection string or URI described in the [Postgres documentation](https://www.postgresql.org/docs/10/static/libpq-connect.html#LIBPQ-CONNSTRING).

#### Running behind a reverse-proxy

If you run the application behind a reverse-proxy, you should tell it what it's externally facing URL is by passing `--route=URL` to `backend`.


## Hacking

Install [Obelisk](https://github.com/obsidiansystems/obelisk) then `ob run`.


### Building with profiling

```
nix-shell -A shells.ghc --arg profiling true
cd backend
cabal new-configure --enable-profiling --enable-library-profiling
cabal new-build
```
