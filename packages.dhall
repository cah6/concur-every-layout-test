
let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190330/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200708/packages.dhall sha256:df5b0f1ae92d4401404344f4fb2a7a3089612c9f30066dcddf9eaea4fe780e29

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
