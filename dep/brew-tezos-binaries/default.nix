{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v9.4-1";
  macos_version = "mojave";
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "11nk79yj32kckbgjaahxdxw2zm6imgs9n6az2h8vqklgh1kblia0";
    };
    tezos-baker-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "05iim7mz7559r7rffkqahjgnmdh9l2qcl73i7fp4p8l7hkws7495";
    };
    tezos-baker-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0hfvjmwy248f9nkm6p8z0gk2xr9n0p3nhfkxmmfgjbfrxb35bbg4";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0i7g81mhxya33qdlszd6hvngri7k6hkxlcjdnq2fpdlg5a8vl96j";
    };
    tezos-endorser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1062gyfczwkrc2lhf1xld2n3wr2pj9404fi2d9p36xydcsilm16g";
    };
    tezos-endorser-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1l23601pn81bm2df2dmdrb2rp63cmy6nhiyjzw09xpwf0vhpkd9x";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1cispfszmvxl3hg7l0kzxifdizdhzskgi6f90m31n6ciy8y5nfrs";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-009-PsFLoren}/${outer-version}/bin/tezos-baker-009-PsFLoren $out/bin/tezos-baker-009-PsFLoren
  chmod +x $out/bin/tezos-baker-009-PsFLoren

  cp ${tezos-baker-010-PtGRANAD}/${outer-version}/bin/tezos-baker-010-PtGRANAD $out/bin/tezos-baker-010-PtGRANAD
  chmod +x $out/bin/tezos-baker-010-PtGRANAD

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-endorser-009-PsFLoren}/${outer-version}/bin/tezos-endorser-009-PsFLoren $out/bin/tezos-endorser-009-PsFLoren
  chmod +x $out/bin/tezos-endorser-009-PsFLoren

  cp ${tezos-endorser-010-PtGRANAD}/${outer-version}/bin/tezos-endorser-010-PtGRANAD $out/bin/tezos-endorser-010-PtGRANAD
  chmod +x $out/bin/tezos-endorser-010-PtGRANAD

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';


  }
