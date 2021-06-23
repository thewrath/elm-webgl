let 
  pkgs = import <nixpkgs> {};
  pkgs_unstable = import <nixpkgs-unstable> {};

  shell = pkgs.mkShell {
    name = "elm-development";

    # The packages in the `buildInputs` list will be added to the PATH in our shell
    buildInputs = [
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      pkgs_unstable.sublime4
    ];
  };  
in shell