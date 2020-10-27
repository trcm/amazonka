{ mkDerivation, aeson, attoparsec, base, bytestring
, case-insensitive, comonad, containers, data-default-class
, deriving-compat, directory-tree, ede, errors, formatting, free
, hashable, haskell-src-exts, html-conduit, lens, mtl
, optparse-applicative, parsec, scientific, semigroups, stdenv
, system-fileio, system-filepath, template-haskell, text, text-icu
, text-manipulate, text-regex-replace, time, transformers
, unexceptionalio, unordered-containers, xml-conduit
}:
mkDerivation {
  pname = "amazonka-gen";
  version = "1.6.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive comonad
    containers data-default-class deriving-compat directory-tree ede
    errors formatting free hashable haskell-src-exts
    html-conduit lens mtl optparse-applicative parsec scientific
    semigroups system-fileio system-filepath template-haskell text
    text-icu text-manipulate text-regex-replace time transformers
    unexceptionalio unordered-containers xml-conduit
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazonka Code Generator";
  license = stdenv.lib.licenses.mpl20;
}
