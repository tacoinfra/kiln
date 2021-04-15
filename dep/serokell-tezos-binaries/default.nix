{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.0-rc2";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "04x4crbc6y2gicmkll4vxx0qpy068xf0x8s64imdbc3xw898bb4k";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
