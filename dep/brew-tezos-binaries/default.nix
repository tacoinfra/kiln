{ pkgs }:
with pkgs;
let
  outer-version = "v16.1-1";
  macos_version = "big_sur";
    tezos-baker-PtLimaPt = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtLimaPt-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0g6qs4xkb4grh16fz59609swg6fj43nxg1rwahqz9dwg39618w7j";
    };
    tezos-baker-PtMumbai = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-baker-PtMumbai-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1dw090d1mfh98zw2ss9wccmz8qp9b4178ab9xqf9kj26a43wi9zd";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-client-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1si74sy5ma3a1x08qylxfdvy0jd6hl48v9xfw3nq95d54f0gdyh4";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/${outer-version}/tezos-node-${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0yvi713dpsqaz01yqhd7kkzp2rv2n8qp0s46i50613pb1p041c5c";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

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
