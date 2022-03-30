{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "12.1-1";

  src = fetchzip {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "08j98fq6n8c3hsm81pjh6j5if1xqhaa02hll8ykcm0hg44ac84gk";
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
