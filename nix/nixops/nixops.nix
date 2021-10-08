{
   pi =
     { config, pkgs, ... }:
     { deployment.targetHost = "10.0.0.165";

       imports = [
         <configuration.nix>
       ]
     };
 }
