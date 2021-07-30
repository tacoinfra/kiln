{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.5";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "1pm74f5f132m8jqgfaysvjxmbnp8slsqpjw15mq2c1pdjp7gzhyc";
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
