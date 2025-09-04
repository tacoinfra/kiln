{ pkgs }:
with pkgs;
let
  outer-version = "23.1";

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";
  src = builtins.fetchTarball {
    # Take from https://gitlab.com/tezos-kiln/kiln/-/packages/45174394
    url = "https://gitlab.com/tezos-kiln/kiln/-/package_files/226117165/download";
    sha256 = "1s2qfbfbh2484v44bib43y1m8h11fkix2g6v1jh5ic56qbdk165d";
  };

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${src}/${outer-version}/bin/octez-baker-PtSeouLo $out/bin/tezos-baker-PtSeouLo
  chmod +x $out/bin/tezos-baker-PtSeouLo

  cp ${src}/${outer-version}/bin/octez-baker-PsRiotum $out/bin/tezos-baker-PsRiotum
  chmod +x $out/bin/tezos-baker-PsRiotum

  cp ${src}/${outer-version}/bin/octez-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${src}/${outer-version}/bin/octez-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
