{ pkgs }:
with pkgs;
let
  outer-version = "v17.1-1";
  macos_version = "big_sur";
    tezos-baker-PtNairob = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtNairob-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0cmpj7qfv64zba0dydysp7zhpg0nj62l35i5jh858hjz1kbm04cz";
    };
    tezos-baker-PtMumbai = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtMumbai-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1pws1s0hsy78akyrvf96bipn6vcdjgyibig38ad44pmw32phbqrj";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0jdniivnbd5lklms73avjcj00cs41iy7di32hq3bcn3zym9gvfpb";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "193frapgks3gycicq1lwr9slf4cf6cj0701a9802mv3lrrjd87jq";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtNairob}/${outer-version}/bin/tezos-baker-PtNairob $out/bin/tezos-baker-PtNairob
  chmod +x $out/bin/tezos-baker-PtNairob

  cp ${tezos-baker-PtMumbai}/${outer-version}/bin/tezos-baker-PtMumbai $out/bin/tezos-baker-PtMumbai
  chmod +x $out/bin/tezos-baker-PtMumbai

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
