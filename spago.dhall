{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, license = "MIT"
, repository = "https://github.com/JordanMartinez/purescript-halogen-hooks-extra"
, dependencies = [ "halogen-hooks", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
