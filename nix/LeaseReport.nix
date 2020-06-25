{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "LeaseReport"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 PrivateStorage.io, LLC";
      maintainer = "exarkun@twistedmatrix.com";
      author = "Jean-Paul Calderone";
      homepage = "https://github.com/PrivateStorageio/LeaseReport#readme";
      url = "";
      synopsis = "A tool to inspect leases on shares on a Tahoe-LAFS storage server.";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      exes = {
        "LeaseReport" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."pathwalk" or (errorHandler.buildDepError "pathwalk"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.; }