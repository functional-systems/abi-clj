let
  nixpkgs = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz") {
      overlays = [ ];
      config = { };
    };

in with nixpkgs;

stdenv.mkDerivation {
  name = "api-dev-env";
  buildInputs = [ ];

  nativeBuildInputs = [ glib libGL xorg.libXtst clojure jdk21 babashka ];

  LD_LIBRARY_PATH = "${glib}/lib:${xorg.libXtst}/lib:${libGL}/lib";

  # Post Shell Hook
  shellHook = ''
    alias backend_repl="clj -J-Dvlaaad.reveal.prefs='{:font-family \"Intel One Mono\" :font-size 20}' -M:repl"

    backend_repl
  '';
}
