{ pkgs }:
with pkgs;
let
  outer-version = "v16.0-1";
  macos_version = "big_sur";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0mnziw7n74kib2il6zbvfcilabhq8li61l53kp4d224kd3c42a8m";
    };
    tezos-baker-PtLimaPt = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtLimaPt-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0hjyn3bp6nrh43pvgv2fwjnag50mbiywvgrshv23g61q96p387gd";
    };
    tezos-baker-PtMumbai = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtMumbai-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "019xsarhbg0jqpac0r215l1m8q22cdzx5d8fbrc87cgk5xy5yma7";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0ilgq2lw4pif004x2rhylivsk79999sryjqrqbpsb73hqpi6fvh9";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "12ijabk7ybz7b6j4l518zm94cwqwln3fpqy5z5gwla6pa5mi59b0";
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

  cp ${tezos-baker-PtMumbai}/${outer-version}/bin/tezos-baker-PtMumbai $out/bin/tezos-baker-PtMumbai
  chmod +x $out/bin/tezos-baker-PtMumbai

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
