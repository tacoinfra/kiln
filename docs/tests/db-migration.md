# Testing Kiln DB migration

## Intent

Kiln uses `groundhog` as an ORM for the Postgresql database. As a result, changes in the
Haskell types may affect the state of the DB columns, which may not be adopted by the
automatical DB migration and cause runtime errors during the Kiln launch process.

To avoid unexpected DB migration errors, we're aiming to provide a way to fill
Kiln DB with some real data and perform a migration from the latest Kiln release to the current
feature branch.

## Test scenario workflow

1. Checkout and build Kiln from the `develop` branch
  ```shell
  git checkout develop
  ```
  Follow instructions from the [`voting.md`](./voting.md) except for the final cleanup to
  fill Kiln DB with some data.

2. Checkout and build your feature branch
  ```shell
  git checkout <feature-branch>
  # This command should be run from the root of the repository
  $(nix-build --no-out-link -A installKiln --argstr closure-compiler-setting "BUNDLE")/bin/install-kiln app-feature
  ```

3. Copy the state of the stable Kiln instance to the feature one
  ```shell
  cp -r app/db app-feature/
  cp -r app/.kiln app-feature/
  ```

4. Start Kiln instance built from your feature branch
  ```shell
  cd app-feature && ./backend --node-config-file <path to the node config> --rights-history-window=10
  ```
  Make sure to check that there are no DB migration errors and adjust it if needed.
