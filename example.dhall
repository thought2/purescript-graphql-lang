let config = ./spago.dhall
in
  { name = "graphql-lang-examples"
  , dependencies = config.dependencies
  , packages = ./packages.dhall
  , sources = config.sources # ["examples/**/*.purs"]
  }
