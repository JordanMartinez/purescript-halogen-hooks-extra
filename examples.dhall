let
 config = ./spago.dhall
in { name = "my-project"
   , dependencies = config.dependencies # [ ]
   , packages = config.packages
   , sources = config.sources # [ "examples/**/*.purs" ]
   }
