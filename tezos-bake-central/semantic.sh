#!/usr/bin/env bash
nix-shell -p nodejs-8_x --pure --command 'cd semantic ; ../node_modules/.bin/gulp build'
