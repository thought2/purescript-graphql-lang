{ name = "graphql-lang"
, dependencies =
  [ "arrays"
  , "maybe"
  , "numbers"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
