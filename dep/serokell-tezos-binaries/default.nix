{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-binaries-${version}";
  version = "7.2-1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "1xxyxnyc8wzliddfqwhj83k44pnhg74a03ii7a9r96rnfwyd9sam";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/multinetwork-$bin
    chmod +x $out/bin/multinetwork-$bin
  done
  '';
  }
