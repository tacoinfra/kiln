{ nixpkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "135a7f9604c482a27caa2a6fff32c6a11a4d9035";
    sha256 = "0x5w33rv8y1zsmhxnac8d2jjp58w00zjhhg7m5pnswghcwgg6aqj";
  }) {}
}: let
  obelisk = import (./.obelisk/impl) {};
  reflex-platform = obelisk.reflex-platform;
  nodeKit-src = import (reflex-platform.hackGet ../dep/tezos-baking-platform) {};
in {
  kits = nixpkgs.stdenv.mkDerivation {
    name = "scoped-tzkits";
    sourceRoot = ".";
    # NO SOURCES
    src = nixpkgs.runCommand "empty" {
      outputHashMode = "recursive";
      outputHashAlgo = "sha256";
      outputHash = "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5";
    } "mkdir $out";
    configurePhase = "true";
    installPhase = "true";
    nativeBuildInputs = [];
    buildInputs = [];
    buildPhase = ''
      mkdir -p $out/bin
      for bin in $(ls ${nodeKit-src.tezos.mainnet.kit}/bin) ; do
        ln -s ${nodeKit-src.tezos.mainnet.kit}/bin/$bin $out/bin/mainnet-$bin
      done
      for bin in $(ls ${nodeKit-src.tezos.alphanet.kit}/bin) ; do
        ln -s ${nodeKit-src.tezos.alphanet.kit}/bin/$bin $out/bin/alphanet-$bin
      done
      for bin in $(ls ${nodeKit-src.tezos.zeronet.kit}/bin) ; do
        ln -s ${nodeKit-src.tezos.zeronet.kit}/bin/$bin $out/bin/zeronet-$bin
      done
      for bin in $(ls ${nodeKit-src.tezos.master.kit}/bin) ; do
        ln -s ${nodeKit-src.tezos.master.kit}/bin/$bin $out/bin/master-$bin
      done

      ls $out/bin
    '';
  };
}
