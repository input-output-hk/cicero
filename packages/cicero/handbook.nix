{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    packages.cicero-handbook = pkgs.runCommand "cicero-handbook" {
      buildInputs = with pkgs; [
        mdbook
        mdbook-mermaid
      ];
    } ''
      mdbook build --dest-dir $out ${inputs.inclusive.lib.inclusive ../.. [
        ../../book.toml
        ../../docs
      ]}
    '';
  };
}
