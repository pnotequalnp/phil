{
  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in rec {
      devShell.x86_64-linux = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.zlib ];
      };
    };
}
