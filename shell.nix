with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "math-test";
  buildInputs = [ readline ncurses ];
}
