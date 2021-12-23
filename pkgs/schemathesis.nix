{ lib, buildPythonPackage, fetchPypi }:

buildPythonPackage rec {
  pname = "schemathesis";
  version = "3.11.7";

  src = fetchPypi {
    inherit pname version;
    sha256 = "e927c5132117916c5af7aeaf6640382ac2b940c9";
  };

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/schemathesis/schemathesis";
    description = "Is a modern API testing tool for web applications built with Open API and GraphQL specifications";
    license = licenses.bsd3;
    maintainers = with maintainers; [ fridh ];
  };
}
