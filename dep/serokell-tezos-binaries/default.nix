{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.3";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "00iix01nng0rqcsydgxvc1yqxszfgv7k33m4xxq44vvrgcpn7wrm";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
