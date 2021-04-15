{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "9.0-rc2";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "1griwfrzb92hkpygg1anpqsi2gvw1vdlg4a48mqdjh6c54bj6kf3";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
