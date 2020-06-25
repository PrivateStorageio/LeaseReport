{
  extras = hackage:
    { packages = { LeaseReport = ./LeaseReport.nix; }; };
  resolver = "lts-14.27";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }