module GraphQL.Language.AST.Encode (printDocument) where

import Prelude hiding (between)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Array.NonEmpty (toArray)
import Data.Int as Int
import Data.Number.Format as Number
import Data.String as String
import GraphQL.Language.AST as AST

printDocument :: AST.Document -> String
printDocument (AST.Document xs) = newlines $ map printDefinition $ toArray xs

printDefinition :: AST.Definition -> String
printDefinition = case _ of
  AST.DefinitionOperation x -> printOperationDefinition x
  AST.DefinitionFragment x -> printFragmentDefinition x

printOperationDefinition :: AST.OperationDefinition -> String
printOperationDefinition = case _ of
  AST.OperationSelectionSet x -> printSelectionSet x
  AST.OperationDefinition type_ name varDefs dirs selSet ->
    printOperationType type_
      <> foldMap spaced name
      <> optEmptyArray (spaced <<< printVariableDefinitions) varDefs
      <> optEmptyArray (spaced <<< printDirectives) dirs
      <> spaced (printSelectionSet selSet)

printVariableDefinitions :: AST.VariableDefinitions -> String
printVariableDefinitions = parens <<< commas <<< map printVariableDefinition

printVariableDefinition :: AST.VariableDefinition -> String
printVariableDefinition (AST.VariableDefinition var inty def) =
  printVariable var
    <> ":"
    <> printInputType inty
    <> foldMap printValue def

printVariable :: AST.Variable -> String
printVariable = printName

printName :: AST.Name -> String
printName = identity

printInputType :: AST.InputType -> String
printInputType = case _ of
  AST.TypeNamed x -> printName x
  AST.TypeList x -> printInputType x
  AST.TypeNonNull x -> printNonNullType x

printNonNullType :: AST.NonNullType -> String
printNonNullType = case _ of
  AST.NonNullTypeNamed x -> printName x
  AST.NonNullTypeList x -> printInputType x

printValue :: AST.Value -> String
printValue = case _ of
  AST.ValueVariable x -> printVariable x
  AST.ValueInt x -> Int.toStringAs Int.decimal x
  AST.ValueFloat x -> Number.toString x
  AST.ValueString x -> "\"" <> x <> "\""
  AST.ValueBoolean x -> printBoolean x
  AST.ValueNull -> "null"
  AST.ValueEnum x -> printName x
  AST.ValueList xs -> brackets $ commas $ map printValue xs
  AST.ValueObject xs -> braces $ commas $ map printObjectField xs

printBoolean :: Boolean -> String
printBoolean = case _ of
  true -> "true"
  false -> "false"

printObjectField :: AST.ObjectField -> String
printObjectField (AST.ObjectField n v) = printName n <> ":" <> printValue v

printDirectives :: AST.Directives -> String
printDirectives = spaces <<< map printDirective

printDirective :: AST.Directive -> String
printDirective (AST.Directive name args) =
  "@"
    <> name
    <> optEmptyArray printArguments args

printSelection :: AST.Selection -> String
printSelection = case _ of
  AST.SelectionField x -> printField x
  AST.SelectionFragmentSpread x -> printFragmentSpread x
  AST.SelectionInlineFragment x -> printInlineFragment x

printSelectionSet :: AST.SelectionSet -> String
printSelectionSet x = braces $ commas $ map printSelection $ toArray x

printField :: AST.Field -> String
printField (AST.Field alias' name args dirs selso) =
  foldMap ((":" <> _) <<< printAlias) alias'
    <> name
    <> optEmptyArray printArguments args
    <> optEmptyArray printDirectives dirs
    <> optEmptyArray printSelectionSetOpt selso

printAlias :: AST.Alias -> String
printAlias (AST.Alias name') = printName name'

printArguments :: AST.Arguments -> String
printArguments = parens <<< commas <<< map printArgument

printArgument :: AST.Argument -> String
printArgument (AST.Argument name v) = name <> ":" <> printValue v

printSelectionSetOpt :: AST.SelectionSetOpt -> String
printSelectionSetOpt x = braces $ commas $ map printSelection x

printFragmentSpread :: AST.FragmentSpread -> String
printFragmentSpread (AST.FragmentSpread name ds) =
  "..."
    <> name
    <> optEmptyArray printDirectives ds

printInlineFragment :: AST.InlineFragment -> String
printInlineFragment (AST.InlineFragment tc dirs sels) =
  "... on " <> foldMap printName tc
    <> printDirectives dirs
    <> printSelectionSet sels

printOperationType :: AST.OperationType -> String
printOperationType = case _ of
  AST.Query -> "query"
  AST.Mutation -> "mutation"

printFragmentDefinition :: AST.FragmentDefinition -> String
printFragmentDefinition (AST.FragmentDefinition name' tc dirs sels) =
  "fragment " <> name' <> " on " <> tc
    <> optEmptyArray printDirectives dirs
    <> printSelectionSet sels

--  Internal
spaced :: String -> String
spaced x = " " <> x

between :: String -> String -> String -> String
between open close x = open <> x <> close

parens :: String -> String
parens = between "(" ")"

brackets :: String -> String
brackets = between "[" "]"

braces :: String -> String
braces = between "{" "}"

quotes :: String -> String
quotes = between "\"" "\""

spaces :: Array String -> String
spaces = String.joinWith " "

newlines :: Array String -> String
newlines = String.joinWith " "

commas :: Array String -> String
commas = String.joinWith ","

optEmpty :: forall a b. Monoid b => (a -> Boolean) -> (a -> b) -> a -> b
optEmpty isEmpty f x = if isEmpty x then mempty else f x

optEmptyArray :: forall a b. Monoid b => (Array a -> b) -> Array a -> b
optEmptyArray = optEmpty Array.null
