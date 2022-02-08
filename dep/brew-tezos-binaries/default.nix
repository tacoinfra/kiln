{ pkgs }:
with pkgs;
let
  outer-version = "v12.0-rc2-1";
  macos_version = "catalina";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1k20q3jknj4w4v31b9ixr18ifnm462cr3mrciirl9piqv2wf56cr";
    };
    tezos-baker-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1x4yqkcc3hwal7b28ng4cm6bc6limih83v1h96zbr6j6qhnij81k";
    };
    tezos-baker-012-Psithaca = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-012-Psithaca-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "15b9kz4bcwp98r61qc7si03f75fa1g1sf9bqswijpmi94db04yav";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "08l2kb39wrvjv89jxmwqk3w5lppqggk7yzz7paqv9h8bzas3773l";
    };
    tezos-endorser-011-PtHangz2 = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-011-PtHangz2-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "01dzg43kfsqfj4h3vfpm23lcd3kcx4w0hdq8vgr99j0dk8z3piyx";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "06q33z3fq1fw04swn28lllb45csx38gx2bjngpbiv7qd92m71d8r";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-011-PtHangz2}/${outer-version}/bin/tezos-baker-011-PtHangz2 $out/bin/tezos-baker-011-PtHangz2
  chmod +x $out/bin/tezos-baker-011-PtHangz2

  cp ${tezos-baker-012-Psithaca}/${outer-version}/bin/tezos-baker-012-Psithaca $out/bin/tezos-baker-012-Psithaca
  chmod +x $out/bin/tezos-baker-012-Psithaca

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-011-PtHangz2}/${outer-version}/bin/tezos-endorser-011-PtHangz2 $out/bin/tezos-endorser-011-PtHangz2
  chmod +x $out/bin/tezos-endorser-011-PtHangz2

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
