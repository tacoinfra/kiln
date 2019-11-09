{ pkgs ? (import ../.obelisk/impl {}).reflex-platform.nixpkgs
, system ? builtins.currentSystem
} :
let
  semantic-ui = ./semantic-ui;
  old-pkgs = import ../dep/old-nixpkgs { inherit system; };
  semantic-ui-env = import ./semantic-ui-env {pkgs = old-pkgs;};
in {
  bakemonitor-semantic-ui = pkgs.runCommand "bakemonitor-semantic-ui" {} ''
    ln -s ${semantic-ui-env.package}/lib/node_modules/semantic-ui/node_modules node_modules
    cp -r ${semantic-ui}/{gulpfile.js,semantic.json,src,tasks} ./
    node_modules/.bin/gulp build

    cp -r dist $out

  '';
} // semantic-ui-env
