{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "12.0";

  src = fetchzip {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "sha256-ELGd3u8iLSfcEkdeZx9f/WukIWg585Qs1uSSAzfLDwE=";
      stripRoot = false;
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
