{ stdenv }:

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "7.3";

  src = builtins.fetchTarball {
      url = "https://tqtezos.bintray.com/bottles-tq/tezos-7.3.catalina.bottle.tar.gz";
      sha256 = "12b7c4hivwnn3lpc5cyznsnvglgjqc37dx2m9cmkzvg6yiippgph";
      };

  installPhase = ''
  mkdir -p $out/bin
  for bin in $(ls ${src}/7.3/bin) ; do
    cp ${src}/7.3/bin/$bin $out/bin/$bin
    chmod +x $out/bin/$bin
  done
  '';
  }
