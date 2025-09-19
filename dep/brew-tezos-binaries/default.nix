{ pkgs }:
with pkgs;
let
  outer-version = "23.2";

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";
  src = builtins.fetchTarball {
    # Take from https://gitlab.com/tezos-kiln/kiln/-/packages/45953303
    url = "https://gitlab.com/tezos-kiln/kiln/-/package_files/230108484/download";
    sha256 = "08sdn48bhjl3zhi0lh3wxw5ga19lqxqxb658c5jy76wpr4ms26ib";
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
