{pkgs, serokell-tezos-binaries }:

let
  binaries = serokell-tezos-binaries;
  in pkgs.runCommand "serokell-tezos-binaries" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${binaries}) ; do
    chmod 755 ${binaries}/$bin
    ln -s ${binaries}/$bin $out/bin/multinetwork-$bin
  done
  ''
