{ pkgs }:
with pkgs;
let
  outer-version = "24.2";

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";
  src = builtins.fetchTarball {
    # Taken from https://gitlab.com/tezos-kiln/kiln/-/packages/56424607
    url = "https://gitlab.com/tezos-kiln/kiln/-/package_files/283238477/download";
    sha256 = "18q3kjlsyvr80mngrs4cfcnzjjhlzvdzzc1yki082x457vpbal2v";
  };

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${src}/${outer-version}/bin/octez-baker-PtSeouLo $out/bin/tezos-baker-PtSeouLo
  cp ${src}/${outer-version}/bin/octez-baker-PtSeouLo $out/bin/octez-baker-PtSeouLo
  chmod +x $out/bin/tezos-baker-PtSeouLo
  chmod +x $out/bin/octez-baker-PtSeouLo

  cp ${src}/${outer-version}/bin/octez-baker-PtTALLiN $out/bin/tezos-baker-PtTALLiN
  cp ${src}/${outer-version}/bin/octez-baker-PtTALLiN $out/bin/octez-baker-PtTALLiN
  chmod +x $out/bin/tezos-baker-PtTALLiN
  chmod +x $out/bin/octez-baker-PtTALLiN

  cp ${src}/${outer-version}/bin/octez-client $out/bin/tezos-client
  cp ${src}/${outer-version}/bin/octez-client $out/bin/octez-client
  chmod +x $out/bin/tezos-client
  chmod +x $out/bin/octez-client

  cp ${src}/${outer-version}/bin/octez-node $out/bin/tezos-node
  cp ${src}/${outer-version}/bin/octez-node $out/bin/octez-node
  chmod +x $out/bin/tezos-node
  chmod +x $out/bin/octez-node
  '';
}
