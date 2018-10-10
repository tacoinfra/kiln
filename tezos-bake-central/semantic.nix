{pkgs ? ((import ./.obelisk/impl {}).reflex-platform.nixpkgs)} :
let
  bakemon-semui = import ./semantic {inherit pkgs;};
in
  pkgs.runCommand "bakemonitor-semantic-ui" {} ''
    cp -r ${bakemon-semui.package}/lib/node_modules/semantic-ui/* .
    node_modules/.bin/gulp build

    cp -r dist $out
  ''
