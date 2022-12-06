{
  ghc
}:

let
  rev = "249c76eb6787420b2178a3ba4cc64c4d9c4a5997"; # pin
  sha256 = "sha256:1lq2py428bfs6vv7qikzgh7hqyxw17c65dnfi35m46kjnidrrcbh";
  pkgsArchive = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256;
  };

  nixpkgs = import pkgsArchive {};
  inherit (nixpkgs) haskell;
in

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [nixpkgs.zlib];
}
