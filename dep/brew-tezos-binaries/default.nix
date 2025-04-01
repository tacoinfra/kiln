{ pkgs }:
with pkgs;
let
  outer-version = "22.0-1";
  macos_version = "ventura";
    tezos-baker-PsQuebec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsQuebec-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0q9x3yg271bb4chywvhixwy4y41i4c0irbfk4z17a50b8d7rsp50";
    };
    tezos-baker-PsRiotum = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsRiotum-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1w13almw6zz2qnw50a4zzjp86hnwvdfgm7lynmilvaim1vz8bldw";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0jgr78vk37maffk99vnsr9cjfgax44mr6ydxbvzmac0cgqjbrrac";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0m3y6kkshvajh7axizvcdfk6v0rm7jj2xqw5q60wazhgs1544p9d";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PsQuebec}/v${outer-version}/bin/tezos-baker-PsQuebec $out/bin/tezos-baker-PsQuebec
  chmod +x $out/bin/tezos-baker-PsQuebec

  cp ${tezos-baker-PsRiotum}/v${outer-version}/bin/tezos-baker-PsRiotum $out/bin/tezos-baker-PsRiotum
  chmod +x $out/bin/tezos-baker-PsRiotum

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
