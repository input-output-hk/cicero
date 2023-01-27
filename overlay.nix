{getSystem, ...}: {
  flake.overlays.default = final: _:
    removeAttrs (getSystem final.system).packages ["default"];
}
