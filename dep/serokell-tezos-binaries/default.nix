{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "8.3";

  src = builtins.fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${version}-1/binaries-${version}-1.tar.gz";
      sha256 = "1cfvw417b1cqniahscd7aqpb9qc8igkz5w0rkrmiznmwzlyamcj0";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}) ; do
    cp ${src}/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
