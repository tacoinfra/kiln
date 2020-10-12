{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.4-1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "01rlfdjlyph4b371l3y24a6gszlp5vcrgjlq99nyp1l55sp2d0a1";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
