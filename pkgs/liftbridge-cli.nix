{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "liftbridge-cli";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "liftbridge-io";
    repo = pname;
    rev = "3f1b33d075149ec86b352f9aa6056cfab2988bcd";
    hash = "sha256-8rp/Wb8VKFd791alA+5aJYX7e5n5w7m4JZR6Y6lhO7A=";
  };

  vendorSha256 = "sha256-3OGLtoMirggJGcutX+howBUYE/cbiZrqCsgan+K+cW8=";

  postInstall = ''
    mv $out/bin/main $out/bin/liftbridge-cli
  '';

  meta = with lib; {
    description = "Allows making requests to a Liftbridge server";
    homepage = "https://github.com/liftbridge-io/liftbridge-cli";
    license = licenses.asl20;
    maintainers = with maintainers; [ manveru ];
  };
}
