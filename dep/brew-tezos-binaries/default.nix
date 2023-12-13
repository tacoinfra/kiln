{ pkgs }:
with pkgs;
let
  outer-version = "v19.0-1";
  macos_version = "monterey";
    tezos-baker-PtNairob = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtNairob-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1widkb1wzrg0aczgcc188kvacp8690df9623qhqm71g0hb4zgaam";
    };
    tezos-baker-Proxford = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-Proxford-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0dj5dzbz038kykndj2pvg0mxpl22fnqf9iblx36qdccbm868v2g5";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "058pyx4b0vh986siw2pl099rnx4lr42d1dymhnmwbiqfvhz24a86";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1lj7an59gk5zrpcsmhmz4avz62wp5z8pwlxxlgbjs5fq8xdzfy08";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtNairob}/${outer-version}/bin/tezos-baker-PtNairob $out/bin/tezos-baker-PtNairob
  chmod +x $out/bin/tezos-baker-PtNairob

  cp ${tezos-baker-Proxford}/${outer-version}/bin/tezos-baker-Proxford $out/bin/tezos-baker-Proxford
  chmod +x $out/bin/tezos-baker-Proxford

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
