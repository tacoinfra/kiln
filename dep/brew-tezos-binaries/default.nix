{ brewDirectory ? /usr/local/Cellar/tezos/7.2/bin }:
# this is just a temporary hack
# PLEASE MAKE SURE THAT THIS FILE
if builtins.pathExists brewDirectory
 then brewDirectory
 else builtins.abort "brew-tezos-binaries.nix: Cannot find tezos-binaries"
