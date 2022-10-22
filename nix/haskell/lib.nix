{ lib, haskellLib }: {

  /* Build a cabal package quickly by supplying a previous build of the
     same package.
  */
  buildIncrementally =
    { regularPackage, previousIncrement ? null }:
      (haskellLib.overrideCabal regularPackage
        (drv: {
          preBuild = lib.optionalString (previousIncrement != null) ''
            mkdir -p dist/build
            tar xzf ${previousIncrement.incremental}/dist.tar.gz -C dist/build
          '';
          postInstall = ''
            mkdir $incremental
            tar czf $incremental/dist.tar.gz -C dist/build \
              --mtime='1970-01-01T00:00:00Z' .
          '';
          preFixup = ''
            # Don't try to strip incremental build outputs
            outputs=(${"\\" + "\${"}outputs[@]/incremental})
          '';
        })
      ).overrideAttrs (finalAttrs: previousAttrs: {
        outputs = previousAttrs.outputs ++ ["incremental"];
      });

}
