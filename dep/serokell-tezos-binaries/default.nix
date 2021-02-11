{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "8.2-1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "0yin1j97x04jzmf7ma8s77lcgyr2kfv1rxm7q6cm4364vfa1x55r";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
