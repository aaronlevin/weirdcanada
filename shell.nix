{ nixpkgs ? (import <nixpkgs> {}) }:
nixpkgs.myEnvFun {
  name = "wyrd-distro";
  buildInputs = [
    nixpkgs.python27
    nixpkgs.python27Packages.fabric
    nixpkgs.mpg123
    nixpkgs.vorbisTools
  ];
}
