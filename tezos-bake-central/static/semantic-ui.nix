{pkgs ? (import ../.obelisk/impl {}).reflex-platform.nixpkgs} :
let
  semantic-ui = ./semantic-ui;
  semantic-ui-env = import ./semantic-ui-env {inherit pkgs;};
in {
  bakemonitor-semantic-ui = pkgs.runCommand "bakemonitor-semantic-ui" {} ''
    ln -s ${semantic-ui-env.package}/lib/node_modules/semantic-ui/node_modules node_modules
    cp -r ${semantic-ui}/{gulpfile.js,semantic.json,src,tasks} ./
    node_modules/.bin/gulp build

    cp -r dist $out

  '';
} // semantic-ui-env
