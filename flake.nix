{
  inputs.sr-libs.url = "git+ssh://git@github.com/seereason/sr-flake?dir=sr-libs&ref=main";

  outputs = { self, sr-libs }: {
    devShells = sr-libs.devShells;
  };
}
