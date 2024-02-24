{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module ModuleMunging
  ( Module(..)
  , buildModule
  , displayModule
  , ModuleFragment(..)
  , ModuleExport(..)
  , ModuleImport(..)
  , ModuleImportStyle(..)
  , ModuleDeclaration(..)
  , DeclName(..)
  , DeclBody(..)
  ) where

import Prelude

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.String (IsString)

import Data.List qualified as List
import Data.Maybe qualified as Maybe

type Module :: Type
data Module = Module
  { moduleName :: String
  , moduleExports :: [String]
  , moduleImports :: [ModuleImport]
  , moduleDeclarations :: [ModuleDeclaration]
  }

buildModule :: String -> ModuleFragment -> Module
buildModule name modFragment =
  Module
    { moduleName = name
    , moduleExports = toExports $ moduleFragmentDeclarations modFragment
    , moduleImports =
        flattenModuleImportGroups
          $ List.groupBy groupModuleImports
          $ List.sort
          $ moduleFragmentImports modFragment
    , moduleDeclarations = moduleFragmentDeclarations modFragment
    }
  where
  toExports :: [ModuleDeclaration] -> [String]
  toExports = filter (not . null) . fmap \case
    ModuleDeclaration shouldExport (DeclName funName) _
      | shouldExport -> funName
      | otherwise -> []

  flattenModuleImportGroups :: [[ModuleImport]] -> [ModuleImport]
  flattenModuleImportGroups = foldMap \case
    [] -> []
    x : xs ->
      case moduleImportStyle x of
        ModuleImportStyleOpen -> pure x
        ModuleImportStyleExplicit ids ->
          pure x
            { moduleImportStyle =
                ModuleImportStyleExplicit
                  $ List.sort
                  $ List.nub
                  $ ids <> idsFromModuleImportGroup (moduleImportStyle <$> xs)
            }
        ModuleImportStyleQualified {} -> pure x

  idsFromModuleImportGroup :: [ModuleImportStyle] -> [String]
  idsFromModuleImportGroup = foldMap \case
    ModuleImportStyleExplicit ids -> ids
    _ -> []

  groupModuleImports :: ModuleImport -> ModuleImport -> Bool
  groupModuleImports x y =
    moduleImportName x == moduleImportName y &&
      case (moduleImportStyle x, moduleImportStyle y) of
        (ModuleImportStyleOpen, ModuleImportStyleOpen) -> True
        (ModuleImportStyleExplicit {}, ModuleImportStyleExplicit {}) -> True
        (ModuleImportStyleQualified qx, ModuleImportStyleQualified qy) -> qx == qy
        (_, _) -> False

displayModule :: Module -> String
displayModule m =
  unlines
    $ "-- Auto-generated - do not manually modify!"
    : "module " <> moduleName
    : "  ( " <> List.intercalate "\n  , " moduleExports
    : "  ) where"
    : mconcat
        [ spacingIfNotNull openImportLines
        , spacingIfNotNull explicitImportLines
        , spacingIfNotNull qualifiedImportLines
        , spacingIfNotNull moduleDeclarationLines
        ]
  where
  spacingIfNotNull :: [String] -> [String]
  spacingIfNotNull xs
    | null xs = []
    | otherwise = [List.intercalate "\n" $ "" : xs]

  openImportLines :: [String]
  openImportLines =
    openImports <&> \n -> "import " <> n

  openImports :: [String]
  openImports =
    moduleImports & Maybe.mapMaybe \case
      ModuleImport { moduleImportName = n, moduleImportStyle = s }
        | ModuleImportStyleOpen <- s -> Just n
      _ -> Nothing

  explicitImportLines :: [String]
  explicitImportLines =
    explicitImports
      & fmap \(n, xs) -> "import " <> n <> " (" <> List.intercalate ", " xs <> ")"

  explicitImports :: [(String, [String])]
  explicitImports =
    moduleImports & Maybe.mapMaybe \case
      ModuleImport { moduleImportName = n, moduleImportStyle = s }
        | ModuleImportStyleExplicit xs <- s -> Just (n, xs)
      _ -> Nothing

  qualifiedImportLines :: [String]
  qualifiedImportLines =
    qualifiedImports
      & fmap \(n, q) ->  "import " <> n <> " qualified as " <> q

  qualifiedImports :: [(String, String)]
  qualifiedImports =
    moduleImports & Maybe.mapMaybe \case
      ModuleImport { moduleImportName = n, moduleImportStyle = s }
        | ModuleImportStyleQualified q <- s -> Just (n, q)
      _ -> Nothing

  moduleDeclarationLines :: [String]
  moduleDeclarationLines =
    zip [0 :: Int ..] moduleDeclarations
      & fmap \case
          (n, ModuleDeclaration _ (DeclName {}) (DeclBody body))
            | n < 1 -> body
            | otherwise -> "\n" <> body

  Module
    { moduleName
    , moduleExports
    , moduleImports
    , moduleDeclarations
    } = m

type ModuleFragment :: Type
data ModuleFragment = ModuleFragment
  { moduleFragmentImports :: [ModuleImport]
  , moduleFragmentDeclarations :: [ModuleDeclaration]
  }

instance Semigroup ModuleFragment where
  (<>) :: ModuleFragment -> ModuleFragment -> ModuleFragment
  mf1 <> mf2 =
    ModuleFragment
      { moduleFragmentImports = moduleFragmentImports mf1 <> moduleFragmentImports mf2
      , moduleFragmentDeclarations = moduleFragmentDeclarations mf1 <> moduleFragmentDeclarations mf2
      }

instance Monoid ModuleFragment where
  mempty :: ModuleFragment
  mempty =
    ModuleFragment
      { moduleFragmentImports = []
      , moduleFragmentDeclarations = []
      }

type ModuleExport :: Type
newtype ModuleExport = ModuleExport String

type ModuleImport :: Type
data ModuleImport = ModuleImport
  { moduleImportName :: String
  , moduleImportStyle :: ModuleImportStyle
  } deriving stock (Eq, Ord)

type ModuleImportStyle :: Type
data ModuleImportStyle
  = ModuleImportStyleOpen
  | ModuleImportStyleExplicit [String]
  | ModuleImportStyleQualified String
  deriving stock (Eq, Ord)

type ModuleDeclaration :: Type
data ModuleDeclaration = ModuleDeclaration Bool DeclName DeclBody

type DeclName :: Type
newtype DeclName = DeclName String deriving newtype (IsString)

type DeclBody :: Type
newtype DeclBody = DeclBody String
