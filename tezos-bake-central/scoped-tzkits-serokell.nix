{pkgs, serokell-tezos-binaries-path }:

let
  binaries-path = serokell-tezos-binaries-path;
  in pkgs.runCommand "serokell-tezos-binaries" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${binaries-path}) ; do
    ln -s ${binaries-path}/$bin $out/bin/multinetwork-$bin
  done
  ''
