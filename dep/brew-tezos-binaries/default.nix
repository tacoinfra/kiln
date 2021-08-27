{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v10.2-1";
  macos_version = "mojave";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "09jfnwvrfzj01xd2r8p9i9lp98kbjp29l41mk7w6l8aqk52xh1rs";
    };
    tezos-baker-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0w4ppa09frag4nnpyn5d63vjxjgql458x14x44z2l87zi6kwjkz0";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1gni14w1k21dd43wnmmxbddsrd1dpyl4wc4ya20iswkcwzjcv75j";
    };
    tezos-endorser-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0h3l3q3h4fdy5kv8nv41sc61cj2f509vbhfavfl5nzv4mzgfpw8c";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1rh1hnfip7zb3pn6k9144hf61m8fmf5hnv5vg62xmi1iz51n9i4z";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-010-PtGRANAD}/${outer-version}/bin/tezos-baker-010-PtGRANAD $out/bin/tezos-baker-010-PtGRANAD
  chmod +x $out/bin/tezos-baker-010-PtGRANAD

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-010-PtGRANAD}/${outer-version}/bin/tezos-endorser-010-PtGRANAD $out/bin/tezos-endorser-010-PtGRANAD
  chmod +x $out/bin/tezos-endorser-010-PtGRANAD

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';


  }
