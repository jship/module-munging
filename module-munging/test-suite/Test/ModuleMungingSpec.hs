{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ModuleMungingSpec
  ( spec
  ) where

import Data.Kind (Type)
import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack)
import ModuleMunging
import Prelude
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "Example" do
    it "empty module fragment" do
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo"
            , moduleExports = []
            , moduleImports = []
            , moduleDeclarations = []
            }
        , expectedDisplay = [__i|
            -- Auto-generated - do not manually modify!
            {-\# LANGUAGE ImportQualifiedPost \#-}
            module Foo
              ( \n  ) where\n
          |]
        , testModuleName = ModuleNameExact "Foo"
        , testModuleFragment = mempty
        }

    it "simple" do
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo.Bar"
            , moduleExports = ["foo"]
            , moduleImports =
                [ ModuleImport "Custom.Prelude" ModuleImportStyleOpen
                , ModuleImport "Foo.Baz" $ ModuleImportStyleExplicit ["baz"]
                , ModuleImport "Qualified.Module.A" $ ModuleImportStyleQualified $ Just "A"
                , ModuleImport "Qualified.Module.B" $ ModuleImportStyleQualified $ Just "Module.B"
                ]
            , moduleDeclarations =
                [ ModuleDeclaration True "foo" $ DeclBody [__i|
                    bar :: Int -> Int
                    bar = quux
                  |]
                , ModuleDeclaration False "quux" $ DeclBody [__i|
                    quux :: Int -> Int
                    quux = Module.B.quux . A.quux
                  |]
                ]
            }
        , expectedDisplay = [__i|
              -- Auto-generated - do not manually modify!
              {-\# LANGUAGE ImportQualifiedPost \#-}
              module Foo.Bar
                ( foo
                ) where

              import Custom.Prelude

              import Foo.Baz (baz)

              import Qualified.Module.A qualified as A
              import Qualified.Module.B qualified as Module.B

              bar :: Int -> Int
              bar = quux

              quux :: Int -> Int
              quux = Module.B.quux . A.quux\n
            |]
        , testModuleName = ModuleNameExact "Foo.Bar"
        , testModuleFragment = ModuleFragment
            { moduleFragmentImports =
                [ ModuleImport "Qualified.Module.B" $ ModuleImportStyleQualified $ Just "Module.B"
                , ModuleImport "Custom.Prelude" ModuleImportStyleOpen
                , ModuleImport "Foo.Baz" $ ModuleImportStyleExplicit ["baz"]
                , ModuleImport "Qualified.Module.A" $ ModuleImportStyleQualified $ Just "A"
                ]
            , moduleFragmentDeclarations =
                [ ModuleDeclaration True "foo" $ DeclBody [__i|
                    bar :: Int -> Int
                    bar = quux
                  |]
                , ModuleDeclaration False "quux" $ DeclBody [__i|
                    quux :: Int -> Int
                    quux = Module.B.quux . A.quux
                  |]
                ]
            }
        }

    it "two fragments" do
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo.Bar"
            , moduleExports = ["foo"]
            , moduleImports =
                [ ModuleImport "Custom.Prelude" ModuleImportStyleOpen
                , ModuleImport "Foo.Baz" $ ModuleImportStyleExplicit ["baz"]
                , ModuleImport "Qualified.Module.A" $ ModuleImportStyleQualified $ Just "A"
                , ModuleImport "Qualified.Module.B" $ ModuleImportStyleQualified $ Just "Module.B"
                , ModuleImport "Qualified.Module.C" $ ModuleImportStyleQualified Nothing
                ]
            , moduleDeclarations =
                [ ModuleDeclaration True "foo" $ DeclBody [__i|
                    bar :: Int -> Int
                    bar = quux
                  |]
                , ModuleDeclaration False "quux" $ DeclBody [__i|
                    quux :: Int -> Int
                    quux = Qualified.Module.C.quux . Module.B.quux . A.quux
                  |]
                ]
            }
        , expectedDisplay = [__i|
              -- Auto-generated - do not manually modify!
              {-\# LANGUAGE ImportQualifiedPost \#-}
              module Foo.Bar
                ( foo
                ) where

              import Custom.Prelude

              import Foo.Baz (baz)

              import Qualified.Module.A qualified as A
              import Qualified.Module.B qualified as Module.B
              import Qualified.Module.C qualified

              bar :: Int -> Int
              bar = quux

              quux :: Int -> Int
              quux = Qualified.Module.C.quux . Module.B.quux . A.quux\n
            |]
        , testModuleName = ModuleNameExact "Foo.Bar"
        , testModuleFragment = mconcat
            [ ModuleFragment
                { moduleFragmentImports =
                    [ ModuleImport "Qualified.Module.B" $ ModuleImportStyleQualified $ Just "Module.B"
                    , ModuleImport "Custom.Prelude" ModuleImportStyleOpen
                    , ModuleImport "Foo.Baz" $ ModuleImportStyleExplicit ["baz"]
                    , ModuleImport "Qualified.Module.A" $ ModuleImportStyleQualified $ Just "A"
                    ]
                , moduleFragmentDeclarations =
                    [ ModuleDeclaration True "foo" $ DeclBody [__i|
                        bar :: Int -> Int
                        bar = quux
                      |]
                    ]
                }
            , ModuleFragment
                { moduleFragmentImports =
                    [ ModuleImport "Qualified.Module.C" $ ModuleImportStyleQualified Nothing
                    , ModuleImport "Custom.Prelude" ModuleImportStyleOpen
                    ]
                , moduleFragmentDeclarations =
                    [ ModuleDeclaration False "quux" $ DeclBody [__i|
                        quux :: Int -> Int
                        quux = Qualified.Module.C.quux . Module.B.quux . A.quux
                      |]
                    ]
                }
            ]
        }

    it "module name from filepath" do
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo"
            , moduleExports = []
            , moduleImports = []
            , moduleDeclarations = []
            }
        , expectedDisplay = [__i|
            -- Auto-generated - do not manually modify!
            {-\# LANGUAGE ImportQualifiedPost \#-}
            module Foo
              ( \n  ) where\n
          |]
        , testModuleName = ModuleNameFromFilePath "Foo.hs"
        , testModuleFragment = mempty
        }
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo.Bar"
            , moduleExports = []
            , moduleImports = []
            , moduleDeclarations = []
            }
        , expectedDisplay = [__i|
            -- Auto-generated - do not manually modify!
            {-\# LANGUAGE ImportQualifiedPost \#-}
            module Foo.Bar
              ( \n  ) where\n
          |]
        , testModuleName = ModuleNameFromFilePath "Foo/Bar.hs"
        , testModuleFragment = mempty
        }
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo.Bar"
            , moduleExports = []
            , moduleImports = []
            , moduleDeclarations = []
            }
        , expectedDisplay = [__i|
            -- Auto-generated - do not manually modify!
            {-\# LANGUAGE ImportQualifiedPost \#-}
            module Foo.Bar
              ( \n  ) where\n
          |]
        , testModuleName = ModuleNameFromFilePath "library/Foo/Bar.hs"
        , testModuleFragment = mempty
        }
      runTest TestCase
        { expectedModule = Module
            { moduleName = "Foo.Bar"
            , moduleExports = []
            , moduleImports = []
            , moduleDeclarations = []
            }
        , expectedDisplay = [__i|
            -- Auto-generated - do not manually modify!
            {-\# LANGUAGE ImportQualifiedPost \#-}
            module Foo.Bar
              ( \n  ) where\n
          |]
        , testModuleName = ModuleNameFromFilePath "library\\Foo\\Bar.hs"
        , testModuleFragment = mempty
        }

type TestCase :: Type
data TestCase = TestCase
  { expectedModule :: Module
  , expectedDisplay :: String
  , testModuleName :: ModuleName
  , testModuleFragment :: ModuleFragment
  }

runTest :: HasCallStack => TestCase -> Expectation
runTest testCase = do
  actualModule `shouldBe` expectedModule
  displayModule actualModule `shouldBe` expectedDisplay
  where
  actualModule = buildModule testModuleName testModuleFragment
  TestCase
    { expectedModule
    , expectedDisplay
    , testModuleName
    , testModuleFragment
    } = testCase
