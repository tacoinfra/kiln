{ pkgs }:
with pkgs;
let
  outer-version = "v15.1-1";
  macos_version = "big_sur";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "19c93a7fn4yml6ljxz8c19wimym4rip18idv024zzham51li4i3v";
    };
    tezos-baker-PtLimaPt = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtLimaPt-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1zx8m6f95y6332di0zlak5pwk71aw1b00j73y7yjppldhqy6gg1b";
    };
    tezos-baker-PtKathma = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtKathma-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "14b4lm3xbmg0lbxxv55139lc1p8c3yi1ply21awfdjnnkv2yq755";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "07lcvx4i1r5qq6i7kb41sajy0g3v7ixqh6x6jbj7lqr4wz4b49ly";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "01pihlvfaygj8ss57k8dm95l7kjxrwqijzac63zijg3jh15xy5rq";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-PtLimaPt}/${outer-version}/bin/tezos-baker-PtLimaPt $out/bin/tezos-baker-PtLimaPt
  chmod +x $out/bin/tezos-baker-PtLimaPt

  cp ${tezos-baker-PtKathma}/${outer-version}/bin/tezos-baker-PtKathma $out/bin/tezos-baker-PtKathma
  chmod +x $out/bin/tezos-baker-PtKathma

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
