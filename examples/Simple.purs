module Example.Simple where

import Prelude
import Data.Maybe (Maybe(..))
import GraphQL.Language.AST as QL

queryActors :: QL.Document
queryActors =
  QL.Document
    $ pure
    $ QL.DefinitionOperation
    $ QL.OperationDefinition QL.Query Nothing [] []
    $ pure
    $ QL.SelectionField
    $ QL.Field Nothing "allActors"
        [ QL.Argument "where"
            $ QL.ValueObject
                [ QL.ObjectField "publish" $ QL.ValueBoolean true
                ]
        ]
        []
        [ QL.SelectionField $ QL.Field Nothing "id" [] [] []
        , QL.SelectionField $ QL.Field Nothing "firstName" [] [] []
        , QL.SelectionField $ QL.Field Nothing "lastName" [] [] []
        ]

setActorName :: String -> String -> QL.Document
setActorName id lastName =
  QL.Document
    $ pure
    $ QL.DefinitionOperation
    $ QL.OperationDefinition QL.Mutation Nothing [] []
    $ pure
    $ QL.SelectionField
    $ QL.Field Nothing "updateActors"
        [ QL.Argument "data"
            $ QL.ValueList
                [ QL.ValueObject
                    [ QL.ObjectField "id" $ QL.ValueString id
                    , QL.ObjectField "data"
                        $ QL.ValueObject
                            [ QL.ObjectField "id"
                                $ QL.ValueString id
                            , QL.ObjectField "lastName"
                                $ QL.ValueString lastName
                            ]
                    ]
                ]
        ]
        []
        [ QL.SelectionField $ QL.Field Nothing "id" [] [] []
        ]
