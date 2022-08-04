# Remove once https://github.com/hashicorp/nomad-driver-podman/pull/183 is merged and arrived in nixpkgs.
{
  lib,
  fetchFromGitHub,
  buildGo117Module,
}:
buildGo117Module rec {
  pname = "nomad-driver-podman";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "dermetfan";
    repo = "nomad-driver-podman";
    rev = "309618ce0839da6ec6b1d4d90a9666ff19c74151";
    sha256 = "sha256-WvXyGspfFwcMPtpqhNDu//IMkN+P3xbZSmHVhXDdq0s=";
  };

  vendorSha256 = "sha256-5PQIWSGSR5vizWEsResBLd//yWs99o/bj5DVpRMBwhA=";

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
}
