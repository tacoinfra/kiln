{ pkgs }:
with pkgs;
let
  outer-version = "20.1-2";
  macos_version = "monterey";
    tezos-baker-PtParisB = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PtParisB-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0865a5fsbiyxr52ka79qrqqhcp87ma02bclx6aba51chvcah5ri4";
    };
    tezos-baker-PsParisC = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-baker-PsParisC-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "0pc30mjycwqjingjj6s6wgjfkl6x8vbyyi17mzgg9zqzygdwpbik";
    };
    tezos-client = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-client-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "120lwiz6zv8dqk9mbh1ms1jk1hd0zx0a2nyxbxvv3vk4sgv74r5h";
    };
    tezos-node = fetchTarball {
      url = "https://github.com/serokell/tezos-packaging/releases/download/v${outer-version}/tezos-node-v${outer-version}.${macos_version}.bottle.tar.gz";
      sha256 = "1q0hdjgrdpckibhy0sng9dxkid86q1qaf2mmqpgqlnj7l6xk0n89";
    };

in stdenv.mkDerivation rec {
  name = "tezos-${outer-version}";

  phases = [ "installPhase" ];

  installPhase = ''
  mkdir -p $out/bin

  cp ${tezos-baker-PtParisB}/v${outer-version}/bin/tezos-baker-PtParisB $out/bin/tezos-baker-PtParisB
  chmod +x $out/bin/tezos-baker-PtParisB

  cp ${tezos-baker-PsParisC}/v${outer-version}/bin/tezos-baker-PsParisC $out/bin/tezos-baker-PsParisC
  chmod +x $out/bin/tezos-baker-PsParisC

  cp ${tezos-client}/v${outer-version}/bin/tezos-client $out/bin/tezos-client
  chmod +x $out/bin/tezos-client

  cp ${tezos-node}/v${outer-version}/bin/tezos-node $out/bin/tezos-node
  chmod +x $out/bin/tezos-node
  '';
}
