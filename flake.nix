{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        s3-archiver = final.callCabal2nix "app" ./. { };
      };
      myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in
    {
      packages.${system}.default = myHaskellPackages.s3-archiver;
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/s3-archiver-exe";
      };
    };
}
