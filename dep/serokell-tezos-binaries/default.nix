{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.2";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "081ix6h9cd1di0qck7jm6z31gbv2avkm5mw4v6xkg8dmc64zs0f3";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
