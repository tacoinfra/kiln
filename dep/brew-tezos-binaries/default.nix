{ pkgs }:
with pkgs;
let
  outer-version = "24.0";

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";
  src = builtins.fetchTarball {
    # Taken from https://gitlab.com/tezos-kiln/kiln/-/packages/51152527
    url = "https://gitlab.com/tezos-kiln/kiln/-/package_files/258619225/download";
    sha256 = "1jnv6x2kcsynv3ssbfiba8pmhl0nm975rqpf0glfi30vl477b3n6";
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
