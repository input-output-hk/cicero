{
  perSystem = {
    pkgs,
    lib,
    ...
  }: {
    # Remove once https://github.com/hashicorp/nomad-driver-podman/pull/183 arrived in nixpkgs.
    packages.nomad-driver-podman = pkgs.buildGoModule rec {
      pname = "nomad-driver-podman";
      version = "0.4.0";

      src = pkgs.fetchFromGitHub {
        owner = "hashicorp";
        repo = "nomad-driver-podman";
        rev = "39a4a50ac151d132bdae58f0458ce57320223c2a";
        sha256 = "sha256-QzjPcv1tAVtWBwru9yoRiD2aIORVORdPlLWZRFJ6Ows=";
      };

      vendorSha256 = "sha256-rGqP7Fzh2zms+GB/XYaoZTnaWQD4GmlJJDYprcH9bZQ=";

      subPackages = ["."];

      # some tests require a running podman service
      doCheck = false;

      meta = with lib; {
        homepage = "https://www.github.com/hashicorp/nomad-driver-podman";
        description = "Podman task driver for Nomad";
        platforms = platforms.linux;
        license = licenses.mpl20;
        maintainers = with maintainers; [dermetfan];
      };
    };
  };
}
