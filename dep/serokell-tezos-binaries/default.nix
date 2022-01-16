{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "11.1";

  src = fetchzip {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "0arbx8vap7pvf7jvgcka7v30c9n5s14fn6qadvg0ly8bmck2jprm";
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
