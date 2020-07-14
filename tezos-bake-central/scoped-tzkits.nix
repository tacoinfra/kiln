{pkgs, tezos-binaries }:
let
  binaries = tezos-binaries;
  in pkgs.runCommand "tezosScopedKit" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${binaries.outPath}) ; do
    cp ${binaries.outPath}/$bin $out/bin/multinetwork-$bin
    chmod +x $out/bin/multinetwork-$bin
  done
  ''
