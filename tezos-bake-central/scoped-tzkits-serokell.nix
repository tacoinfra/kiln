{pkgs, serokell-tezos-binaries }:

let
  binaries = serokell-tezos-binaries;
  in pkgs.runCommand "serokell-tezos-binaries" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${binaries}) ; do
    ln -s ${binaries}/$bin $out/bin/multinetwork-$bin
  done
  ''
