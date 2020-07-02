{pkgs, tezos-binaries }:

let
  binaries = tezos-binaries;
  in pkgs.runCommand "tezosScopedKit_" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${binaries}) ; do
    chmod 755 ${binaries}/$bin
    ln -s ${binaries}/$bin $out/bin/multinetwork-$bin
  done
  ''
