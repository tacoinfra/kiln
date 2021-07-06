{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.4";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "0mazzsgdvswix06c7iimgycdja286yx5cagqih2vs73cyycb0yrs";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
