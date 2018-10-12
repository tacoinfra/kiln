#!/usr/bin/env bash
rm node_modules
ln -sf $(nix-build ../semantic-ui.nix -A package --no-out-link)/lib/node_modules/semantic-ui/node_modules node_modules
./node_modules/.bin/gulp "$@"
