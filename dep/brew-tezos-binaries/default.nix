{ pkgs }:
# https://github.com/serokell/tezos-packaging/releases/download/v9.0-rc1-1/tezos-accuser-009-PsFLoren-v9.0-rc1-1.${macos_version}.bottle.tar.gz
with pkgs;
let
  outer-version = "v9.2-1";
  macos_version = "mojave";
    tezos-accuser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0ha70mxvf9m6isnxpc3w27crj6jq9lgzfpfk3317pk66xmrannvz";
    };
    tezos-accuser-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-accuser-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1xg17m5xm2s3f2biyjrzjch3gxmpl4rryn55v0k0wxci62hv8f7v";
    };
    tezos-admin-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-admin-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "16jfd7da3j2bsi47byhks8lq6c5rg995748j94c9zyqn5ijczj3g";
    };
    tezos-baker-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0f7md72654sx3l95yl63j5nxkwchjvna0v8pgr1bb1faxxy0nlp9";
    };
    tezos-baker-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1wgrkk6jmxwnpapp2fb062arbrk8whc2f8crg1ygs60zvh3xb1gv";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "17b00ccfyccrkrxx08pbbb3mwrffpigbymh1dsx9ir00dikh6n0m";
    };
    tezos-codec = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-codec-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "04grk4fk42qh7fzpaqmhnw0g5szx2iq73z5rinffd009ia9c9y7f";
    };
    tezos-endorser-009-PsFLoren = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-009-PsFLoren-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "06ywyvp9djxz717kmfln4hbjic4ymgddxwfviwhy94xw6l2imh3g";
    };
    tezos-endorser-010-PtGRANAD = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-endorser-010-PtGRANAD-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1s5szj4z1d9xgiwzz7jl30c5zy14zk8lshkarf45s2l99mxjak2i";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1r4f042jb3hmkims41dzza40vlahy1dlfsdq5py64yn9qsbz3vfn";
    };
    tezos-signer = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-signer-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1qv540r1z545qpvij53a5xbjh0lllr9bdq4nnazr18968rkwl3mk";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-accuser-009-PsFLoren}/${outer-version}/bin/tezos-accuser-009-PsFLoren $out/bin/tezos-accuser-009-PsFLoren
  chmod +x $out/bin/tezos-accuser-009-PsFLoren

  cp ${tezos-accuser-010-PtGRANAD}/${outer-version}/bin/tezos-accuser-010-PtGRANAD $out/bin/tezos-accuser-010-PtGRANAD
  chmod +x $out/bin/tezos-accuser-010-PtGRANAD

  cp ${tezos-admin-client}/${outer-version}/bin/tezos-admin-client $out/bin/tezos-admin-client
  chmod +x $out/bin/tezos-admin-client

  cp ${tezos-baker-009-PsFLoren}/${outer-version}/bin/tezos-baker-009-PsFLoren $out/bin/tezos-baker-009-PsFLoren
  chmod +x $out/bin/tezos-baker-009-PsFLoren

  cp ${tezos-baker-010-PtGRANAD}/${outer-version}/bin/tezos-baker-010-PtGRANAD $out/bin/tezos-baker-010-PtGRANAD
  chmod +x $out/bin/tezos-baker-010-PtGRANAD

  cp ${tezos-client}/${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-codec}/${outer-version}/bin/tezos-codec $out/bin/tezos-codec
  chmod +x $out/bin/tezos-codec

  cp ${tezos-endorser-009-PsFLoren}/${outer-version}/bin/tezos-endorser-009-PsFLoren $out/bin/tezos-endorser-009-PsFLoren
  chmod +x $out/bin/tezos-endorser-009-PsFLoren

  cp ${tezos-endorser-010-PtGRANAD}/${outer-version}/bin/tezos-endorser-010-PtGRANAD $out/bin/tezos-endorser-010-PtGRANAD
  chmod +x $out/bin/tezos-endorser-010-PtGRANAD

  cp ${tezos-node}/${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node

  cp ${tezos-signer}/${outer-version}/bin/tezos-signer $out/bin/tezos-signer
  chmod +x $out/bin/tezos-signer
  '';


  }
