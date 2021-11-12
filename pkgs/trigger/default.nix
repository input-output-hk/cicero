{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  pname = "trigger";
  version = "1.1.2";

  src = fetchFromGitHub {
    owner = "RedL0tus";
    repo = "trigger";
    rev = version;
    hash = "sha256-Ut/lRIIUT5mq4ZQhbzKOsfdWmao9wbFVXiOym2M3e8o=";
  };

  cargoPatches = [
    ./Cargo.lock.patch
  ];

  cargoHash = "sha256-1QFM0G0if57BZ0kmSvDzV5nH4DiD7l/FqgFEjHhZeu0=";

  meta = with lib; src.meta // {
    description = "Yet another GitHub/GitLab webhook listener";
    license = licenses.mit;
    maintainers = with maintainers; [ dermetfan ];
  };
}
