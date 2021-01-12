{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "8.1-1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "1gcxihqw61b541zmwpkqklbbj2zddkzcmg9i937kx14h914kc03m";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
