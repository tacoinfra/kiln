{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.0-rc1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-4.tar.gz";
      sha256 = "0cwdq24999y0z9vgx0dbwnvjwpckihji293yjyz3sk8cymp4pbh0";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
