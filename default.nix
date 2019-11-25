with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "mini-haskell";
  src = ./.;
  doCheck = true;
  buildInputs = [ gnumake gcc ];
  checkPhase = ''
     ./check_compile.sh classy.hs
  '';
  installPhase = ''
    mkdir -p $out
    cp blynn $out/bin
  '';
}
