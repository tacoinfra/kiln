{ pkgs, tezos-baking-platform }:
let
  tz = tezos-baking-platform.tezos;
in pkgs.runCommand "scoped-tzkits" {} ''
  mkdir -p $out/bin
  for bin in $(ls ${tz.mainnet.kit}/bin) ; do
    ln -s ${tz.mainnet.kit}/bin/$bin $out/bin/mainnet-$bin
  done
  for bin in $(ls ${tz.babylonnet.kit}/bin) ; do
    ln -s ${tz.babylonnet.kit}/bin/$bin $out/bin/babylonnet-$bin
  done
  for bin in $(ls ${tz.alphanet.kit}/bin) ; do
    ln -s ${tz.alphanet.kit}/bin/$bin $out/bin/alphanet-$bin
  done
  for bin in $(ls ${tz.zeronet.kit}/bin) ; do
    ln -s ${tz.zeronet.kit}/bin/$bin $out/bin/zeronet-$bin
  done
  for bin in $(ls ${tz.master.kit}/bin) ; do
    ln -s ${tz.master.kit}/bin/$bin $out/bin/master-$bin
  done
''
