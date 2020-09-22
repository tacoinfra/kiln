{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.4";

  src = builtins.fetchTarball {
      url = "https://github.com/tqtezos/homebrew-tq/releases/download/v7.4/tezos-7.4.catalina.bottle.2.tar.gz";
      sha256 = "0xljw7h0cgy631pf8wq3ziz4g7wkfw7hs6jrbng7viadd8qnh7yn";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}/local/Cellar/tezos/7.4/bin) ; do
    cp ${src}/local/Cellar/tezos/7.4/bin/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
