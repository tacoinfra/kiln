{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "13.0-rc1-2";

  src = fetchzip {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "0fdbjkn3dhdcj0rnbr7jkpjijzg1mzvrdznwcx1jyvd7njyflpwz";
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
