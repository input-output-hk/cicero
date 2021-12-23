{ lib, buildPythonPackage, fetchPypi }:

buildPythonPackage rec {
  pname = "schemathesis";
  version = "3.11.7";

  src = fetchPypi {
    inherit pname version;
    sha256 = "08fdd5ef7c96480ad11c12d472de21acd32359996f69a5259299b540feba4560";
  };

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/schemathesis/schemathesis";
    description = "Is a modern API testing tool for web applications built with Open API and GraphQL specifications";
    license = licenses.bsd3;
    maintainers = with maintainers; [ fridh ];
  };
}
