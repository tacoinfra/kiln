{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.0";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "1rxswqb184jgff2x2dkxp3n4047185k0qsm87l41ldqyfkvnnzlg";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
