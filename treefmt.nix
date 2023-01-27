{inputs, ...}: {
  imports = with inputs; [
    treefmt-nix.flakeModule
  ];

  perSystem = {
    config,
    lib,
    ...
  }: {
    treefmt = {
      projectRootFile = "flake.nix";

      programs = {
        alejandra.enable = true;
        gofmt.enable = true;
      };

      settings.formatter.cue = {
        command = lib.getExe config.packages.treefmt-cue;
        includes = ["*.cue"];
      };
    };
  };
}
