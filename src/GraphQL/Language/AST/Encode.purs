module GraphQL.Language.AST.Encode where

import GraphQL.Language.AST (Alias(..), Argument(..), Arguments, Definition(..), Directive(..), Directives, Document(..), Field(..), FragmentDefinition(..), FragmentSpread(..), InlineFragment(..), InputType(..), Name, NonNullType(..), ObjectField(..), OperationDefinition(..), OperationType(..), Selection(..), SelectionSet, SelectionSetOpt, Value(..), Variable, VariableDefinition(..), VariableDefinitions)
import Prelude (class Monoid, identity, map, mempty, ($), (<<<), (<>))
import Data.Array (foldMap)
import Data.Array as Array
import Data.Int as Int
import Data.Number.Format as Number
import Data.String as String

document :: Document -> String
document (Document xs) = newlines $ map definition xs

definition :: Definition -> String
definition = case _ of
  DefinitionOperation x -> operationDefinition x
  DefinitionFragment x -> fragmentDefinition x

operationDefinition :: OperationDefinition -> String
operationDefinition = case _ of
  OperationSelectionSet x -> selectionSet x
  OperationDefinition type_ name varDefs dirs selSet ->
    operationType type_
      <> foldMap spaced name
      <> optEmpty Array.null (spaced <<< variableDefinitions) varDefs
      <> optEmpty Array.null (spaced <<< directives) dirs
      <> optEmpty Array.null (spaced <<< selectionSet) selSet

variableDefinitions :: VariableDefinitions -> String
variableDefinitions = parens <<< commas <<< map variableDefinition

variableDefinition :: VariableDefinition -> String
variableDefinition (VariableDefinition var inty def) =
  variable var
    <> ":"
    <> inputType inty
    <> foldMap value def

variable :: Variable -> String
variable = name

name :: Name -> String
name = identity

inputType :: InputType -> String
inputType = case _ of
  TypeNamed x -> name x
  TypeList x -> inputType x
  TypeNonNull x -> nonNullType x

nonNullType :: NonNullType -> String
nonNullType = case _ of
  NonNullTypeNamed x -> name x
  NonNullTypeList x -> inputType x

value :: Value -> String
value = case _ of
  ValueVariable x -> variable x
  ValueInt x -> Int.toStringAs Int.decimal x
  ValueFloat x -> Number.toString x
  ValueString x -> "\"" <> x <> "\""
  ValueBoolean x -> case x of
    true -> "true"
    false -> "false"
  ValueNull -> "null"
  ValueEnum x -> name x
  ValueList xs -> brackets $ commas $ map value xs
  ValueObject xs -> braces $ commas $ map objectField xs

objectField :: ObjectField -> String
objectField (ObjectField n v) = name n <> ":" <> value v

directives :: Directives -> String
directives = spaces <<< map directive

directive :: Directive -> String
directive (Directive name args) = "@" <> name <> optEmpty Array.null arguments args

selection :: Selection -> String
selection = case _ of
  SelectionField x -> field x
  SelectionFragmentSpread x -> fragmentSpread x
  SelectionInlineFragment x -> inlineFragment x

selectionSet :: SelectionSet -> String
selectionSet x = braces $ commas $ map selection x

field :: Field -> String
field (Field alias' name args dirs selso) =
  foldMap ((":" <> _) <<< alias) alias'
    <> name
    <> optEmpty Array.null arguments args
    <> optEmpty Array.null directives dirs
    <> optEmpty Array.null selectionSetOpt selso

alias :: Alias -> String
alias (Alias name') = name name'

arguments :: Arguments -> String
arguments = parens <<< commas <<< map argument

argument :: Argument -> String
argument (Argument name v) = name <> ":" <> value v

selectionSetOpt :: SelectionSetOpt -> String
selectionSetOpt x = braces $ commas $ map selection x

fragmentSpread :: FragmentSpread -> String
fragmentSpread (FragmentSpread name ds) =
  "..."
    <> name
    <> optEmpty Array.null directives ds

inlineFragment :: InlineFragment -> String
inlineFragment (InlineFragment tc dirs sels) =
  "... on " <> foldMap name tc
    <> directives dirs
    <> selectionSet sels

operationType :: OperationType -> String
operationType = case _ of
  Query -> "query"
  Mutation -> "mutation"

fragmentDefinition :: FragmentDefinition -> String
fragmentDefinition (FragmentDefinition name' tc dirs sels) =
  "fragment " <> name' <> " on " <> tc
    <> optEmpty Array.null directives dirs
    <> selectionSet sels

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
