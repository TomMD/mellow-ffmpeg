let
  git = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/603e89eef935b22754e1406364ba0921997c71c4.tar.gz";
    sha256 = "sha256:0b3wmjc5q7mv1y0gkg2m8b6kr5vakakkcrn022cpmdqlk7f097xx";
  }) {};
in new.mkShell {
  buildInputs = with git; [
    ghc
    haskellPackages.haskell-language-server
    neovim
    zlib.dev
    libGL.dev
    libGLU.dev
    (ffmpeg-headless.override { buildAvutil = true; })
    pkg-config
  ];
}
