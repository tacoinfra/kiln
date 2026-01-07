{ pkgs }:
with pkgs;
let
  outer-version = "24.0-rc2";

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";
  src = builtins.fetchTarball {
    # Taken from https://gitlab.com/tezos-kiln/kiln/-/packages/50693297
    url = "https://gitlab.com/tezos-kiln/kiln/-/package_files/256151674/download";
    sha256 = "0a21yhfb0qbwa83f3h5sgi6cms015yn9ccmynca5cvar578dhjba";
  };

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${src}/${outer-version}/bin/octez-baker-PtSeouLo $out/bin/tezos-baker-PtSeouLo
  chmod +x $out/bin/tezos-baker-PtSeouLo

  cp ${src}/${outer-version}/bin/octez-baker-PtTALLiN $out/bin/tezos-baker-PtTALLiN
  chmod +x $out/bin/tezos-baker-PtTALLiN

  cp ${src}/${outer-version}/bin/octez-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${src}/${outer-version}/bin/octez-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
