{pkgs, tezos-binaries }:
let
  binaries = tezos-binaries;
  in pkgs.runCommand "tezosScopedKit" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${binaries}) ; do
    cp ${binaries}/$bin $out/bin/multinetwork-$bin
    chmod +x $out/bin/multinetwork-$bin
  done
  ''
