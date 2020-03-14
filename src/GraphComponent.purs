module GraphComponent where

import AppState (AppState, emptyAppState, Action(..), Input, Message, Query)
import GraphComponent.HandleAction (handleAction, handleQuery)
import GraphComponent.Render (render)

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH


initialState :: Input -> AppState
initialState windowShape =
  emptyAppState { left : 0.0
                , width : windowShape.width
                , right : windowShape.height
                , top : 0.0
                , height : windowShape.height
                , bottom : windowShape.width
                }

graphComponent :: H.Component HH.HTML Query Input Message Aff
graphComponent =
  H.mkComponent
    { initialState : initialState
    , render : render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    , initialize   = Just Init
                                    })
    }
