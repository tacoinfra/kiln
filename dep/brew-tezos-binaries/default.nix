{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.3";

  src = builtins.fetchTarball {
      url = "https://tqtezos.bintray.com/bottles-tq/tezos-7.3.catalina.bottle.tar.gz"
      ; sha256 = "0mmx49ppzk036wwi3v76siiahhkqyp2zidm5zfb7x7b1pzkpmgmz";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}/7.3/bin) ; do
    cp ${src}/7.3/bin/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
