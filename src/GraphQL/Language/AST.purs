module GraphQL.Language.AST where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Eq (class Eq)
import Data.Maybe (Maybe)

type Name
  = String

data Document
  = Document Definitions

type Definitions
  = NonEmptyArray Definition

data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition

-- | Operations
--
--
data OperationDefinition
  = OperationSelectionSet SelectionSet
  | OperationDefinition OperationType (Maybe Name) VariableDefinitions Directives SelectionSet

data OperationType
  = Query
  | Mutation

-- | SelectionSet
--
--
type SelectionSet
  = NonEmptyArray Selection

type SelectionSetOpt
  = Array Selection

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

-- | Field
--
--
data Field
  = Field (Maybe Alias) Name Arguments Directives SelectionSetOpt

data Alias
  = Alias Name

-- | Arguments
--
--
type Arguments
  = Array Argument

data Argument
  = Argument Name Value

-- | Fragments
--
--
data FragmentSpread
  = FragmentSpread Name Directives

data InlineFragment
  = InlineFragment (Maybe TypeCondition) Directives SelectionSet

data FragmentDefinition
  = FragmentDefinition FragmentName TypeCondition Directives SelectionSet

type FragmentName
  = Name

type TypeCondition
  = Name

-- | Input Values
--
--
data Value
  = ValueVariable Variable
  | ValueInt Int
  | ValueFloat Number
  | ValueString String
  | ValueBoolean Boolean
  | ValueNull
  | ValueEnum Name
  | ValueList (Array Value)
  | ValueObject (Array ObjectField)

data ObjectField
  = ObjectField Name Value

-- | Variables
--
--
type VariableDefinitions
  = Array VariableDefinition

data VariableDefinition
  = VariableDefinition Variable InputType (Maybe DefaultValue)

type Variable
  = Name

type DefaultValue
  = Value

-- | Input Types
--
--
data InputType
  = TypeNamed Name
  | TypeList InputType
  | TypeNonNull NonNullType

data NonNullType
  = NonNullTypeNamed Name
  | NonNullTypeList InputType

-- | Directives
--
--
type Directives
  = Array Directive

data Directive
  = Directive Name (Array Argument)
