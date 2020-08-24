{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.3-1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}/binaries-${version}.tar.gz";
      sha256 = "039qal3j320vkv5hri4sv399jch8smznaxx4x24phrb5i6gh61p1";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
