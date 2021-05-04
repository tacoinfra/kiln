{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.1";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "13ckx0xvgvdlwl33ihdd12qydirjl4b5dd655i0l8vgp4fzm3ybi";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
