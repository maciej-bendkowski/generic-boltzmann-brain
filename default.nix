{ mkDerivation, base, containers, mtl, multiset, process
, lib, tasty, tasty-hunit
# python package needed at test and run time, undeclared in .cabal file
, paganini-hs, paganini
}:
mkDerivation {
  pname = "generic-boltzmann-brain";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers mtl multiset process tasty tasty-hunit
  ];
  testHaskellDepends = [
    base
    tasty
    tasty-hunit
    paganini-hs
    paganini
  ];
  homepage = "https://github.com/maciej-bendkowski/generic-boltzmann-brain#readme";
  license = lib.licenses.bsd3;
}
