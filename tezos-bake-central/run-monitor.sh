#!/usr/bin/env bash
# little helper script to launch the baking monitor

set -eux

backendDrv=$(nix-build -A exe --no-out-link)

ln -sft . "$backendDrv"/frontend.jsexe

exec "$backendDrv"/backend
