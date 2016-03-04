{ nixpkgs ? (import <nixpkgs> {}) }:
nixpkgs.myEnvFun {
  name = "wyrd-distro";
  buildInputs = [
    nixpkgs.python27
    nixpkgs.python27Packages.Fabric
    nixpkgs.mpg123
    nixpkgs.vorbisTools
  ];
}
