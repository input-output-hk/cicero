{ id, inputs ? { }, inputsJSON ? null }:
let
  inherit (builtins)
    all attrNames attrValues concatStringsSep fromJSON functionArgs getFlake
    hashString;

  processedInputs = inputs
    // (if inputsJSON != null then (fromJSON inputsJSON) else { });

  inherit ((getFlake (toString ./.)).legacyPackages.x86_64-linux)
    lib liftbridge-cli bash coreutils;

  task = name:
    { when ? { ... }: { }, success ? { ${name} = true; }
    , failure ? { ${name} = false; }, ... }@args:
    givenScript:
    let
      evaluated = when processedInputs;
      ok = all (a: a) (attrValues evaluated);
      drv = derivation rec {
        inherit id name;
        passAsFile = [ "script" ];
        script = givenScript;
        system = "x86_64-linux";
        result =
          concatStringsSep "." [ id name (hashString outputHashAlgo script) ];
        outputHashAlgo = "sha256";
        outputHashMode = "flat";
        outputHash = hashString outputHashAlgo result;
        PATH = lib.makeBinPath [ liftbridge-cli bash coreutils ];
        builder = ./builder.sh;
      };
    in {
      inputs = attrNames (functionArgs when);
      when = evaluated;
      run = if ok then drv else null;
      inherit success failure;
    };
in {
  ping = task "ping" {
    when = { ping ? false, pong ? false, ... }: {
      "ping missing" = !ping;
      "pong missing" = !pong;
    };
  } ''
    echo running ping
    sleep 10
    liftbridge-cli p -s workflow.${id}.cert -c -m '{"ping": true}'
  '';

  pong = task "pong" {
    when = { ping ? false, pong ? false, ... }: {
      "ping sent" = ping;
      "pong missing" = !pong;
    };
  } ''
    echo running pong
    sleep 10
    liftbridge-cli p -s workflow.${id}.cert -c -m '{"pong": true}'
  '';

  pingpong = task "pingpong" {
    when = { ping ? false, pong ? false, ... }: {
      "ping sent" = ping;
      "pong sent" = pong;
    };
  } ''
    echo running pingpong
    sleep 10
    liftbridge-cli p -s workflow.${id}.cert -c -m '{"pingpong": true}'
  '';
}
