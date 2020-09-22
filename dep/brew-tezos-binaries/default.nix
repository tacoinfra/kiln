{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.4";

  src = builtins.fetchTarball {
      url = "https://github.com/tqtezos/homebrew-tq/releases/download/v7.4/tezos-7.4.catalina.bottle.2.tar.gz";
      sha256 = "069agbp4qqlbw54wqq9x9azp6l32kpklsgikpvql0v67k6x9xq09";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}/7.4/bin) ; do
    cp ${src}/7.4/bin/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
