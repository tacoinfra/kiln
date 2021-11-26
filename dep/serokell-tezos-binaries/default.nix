{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "11.0+no_adx";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "0jpnzckkpck5hqc03gakwf0mll9d1kkdx4ya7alsnjn9ds6ycizb";
      };
  binaries = ["tezos-client" "tezos-node" "tezos-baker-*" "tezos-endorser-*" "tezos-admin-client"];
  installPhase = ''
  mkdir -p $out/bin
  for bin in $binaries ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
