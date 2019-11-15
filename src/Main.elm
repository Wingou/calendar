module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, table, td, text, tr)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import List exposing (filter, head, map, range)
import String exposing (fromInt, toInt)



---- MODEL ----


type alias ZIndex =
    { id : Int
    , z : Int
    }


type alias Model =
    { zDoor : List ZIndex
    , zImage : List ZIndex
    }


intialModel : Model
intialModel =
    { zDoor = map (\r -> { id = r, z = 1 }) (range 1 25)
    , zImage = map (\r -> { id = r, z = 0 }) (range 1 25)
    }


init : ( Model, Cmd Msg )
init =
    ( intialModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | DoorOpen Int
    | DoorClose Int



----------------------------
-------- Functions ---------


zDoorOpen : Int -> List ZIndex -> List ZIndex
zDoorOpen i zIndex =
    map
        (\zi ->
            { zi
                | z =
                    if zi.id == i then
                        zi.z + 2

                    else
                        zi.z
            }
        )
        zIndex


zFromId : Int -> List ZIndex -> Int
zFromId i zIndex =
    case head <| filter (\zi -> i == zi.id) zIndex of
        Just z ->
            z.z

        Nothing ->
            0


coord : Int -> Int -> Int
coord column line =
    column * nbColumns + line - nbColumns


displayColumn : Int -> Int -> Model -> List (Html Msg)
displayColumn column line model =
    map (\r -> tr [] (displayLine column line r model)) (range 1 line)


displayLine : Int -> Int -> Int -> Model -> List (Html Msg)
displayLine column line indiceColumn model =
    map
        (\r ->
            td []
                [ let
                    i =
                        coord indiceColumn r

                    zI =
                        zFromId i model.zImage

                    zD =
                        zFromId i model.zDoor
                  in
                  div []
                    [ text (fromInt i)
                    , displayImage i zI
                    , displayDoor i zD
                    ]
                ]
        )
        (range 1 column)


displayTable : Int -> Int -> Model -> Html Msg
displayTable column line model =
    table [] (displayColumn column line model)


displayImage : Int -> Int -> Html Msg
displayImage i z =
    div [ style "z-index" (fromInt z), style "position" "absolute", style "width" "100px", style "height" "100px" ]
        [ img
            [ onClick (DoorClose i)
            , style "height" "100px"
            , src ("images/" ++ fromInt i ++ ".jpg")
            ]
            []
        ]


displayDoor : Int -> Int -> Html Msg
displayDoor i z =
    div [ style "z-index" (fromInt z), style "position" "relative", style "width" "100px", style "height" "100px" ]
        [ img
            [ onClick (DoorOpen i)
            , style "height" "100px"
            , src "images/door.png"
            ]
            []
        ]



-------- Functions ---------
----------------------------
----------------------------
------ CONSTANTES ----------


nbColumns : Int
nbColumns =
    5


nbLines : Int
nbLines =
    5



------ CONSTANTES ----------
----------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoorOpen i ->
            ( { model
                | zImage = zDoorOpen i model.zImage
              }
            , Cmd.none
            )

        DoorClose i ->
            ( { model
                | zDoor = zDoorOpen i model.zDoor
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ displayTable nbColumns nbLines model ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
