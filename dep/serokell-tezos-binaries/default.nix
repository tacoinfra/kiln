{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.4-1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "0xnrcyb0x818m7vw1d30706hzkma15drj8iry47dr8jc2c817d0n";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
