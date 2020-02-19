# DO NOT HAND-EDIT THIS FILE
let fetch = {url, rev, ref ? null, sha256 ? null, fetchSubmodules ? false, private ? false, ...}:
  let realUrl = let firstChar = builtins.substring 0 1 url; in
    if firstChar == "/" then /. + url
    else if firstChar == "." then ./. + url
    else url;
  in if !fetchSubmodules && private then builtins.fetchGit {
    url = realUrl; inherit rev;
  } else (import <nixpkgs> {}).fetchgit {
    url = realUrl; inherit rev sha256;
  };
in import (fetch (builtins.fromJSON (builtins.readFile ./git.json)))
