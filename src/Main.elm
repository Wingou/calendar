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
    { selectedDoor : Int
    , selectedImage : Int
    , zDoor : List ZIndex
    , zImage : List ZIndex
    }


intialModel : Model
intialModel =
    { selectedDoor = 0
    , selectedImage = 0
    , zDoor = map (\r -> { id = r, z = 1 }) (range 1 25)
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
    | ZoomImage Int
    | ResetSelectedImage Int



----------------------------
-------- Functions ---------


imgHeight : String
imgHeight =
    "120px"


imgWidth : String
imgWidth =
    "120px"


imgZoomWidth : String
imgZoomWidth =
    "622px"


imgZoomHeight : String
imgZoomHeight =
    "642px"


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
                    [ displayImage i zI
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
    div
        [ style "z-index" (fromInt z)
        , style "position" "absolute"
        , style "width" imgWidth
        , style "height" imgHeight
        , style "background-color" "WHITE"
        ]
        [ img
            [ onClick (ZoomImage i)
            , style "height" imgHeight
            , src ("images/img/" ++ fromInt i ++ ".jpg")
            ]
            []
        ]


displayDoor : Int -> Int -> Html Msg
displayDoor i z =
    div [ style "z-index" (fromInt z), style "position" "relative", style "width" imgWidth, style "height" imgHeight ]
        [ img
            [ onClick (DoorOpen i)
            , style "height" imgHeight
            , src ("images/door/door" ++ fromInt i ++ ".jpg")
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
                | selectedDoor = i
                , zImage = zDoorOpen i model.zImage
                , zDoor =
                    if model.selectedDoor /= i then
                        zDoorOpen model.selectedDoor model.zDoor

                    else
                        model.zDoor
              }
            , Cmd.none
            )

        DoorClose i ->
            ( { model
                | zDoor = zDoorOpen i model.zDoor
              }
            , Cmd.none
            )

        ZoomImage i ->
            ( { model
                | selectedImage = i
              }
            , Cmd.none
            )

        ResetSelectedImage i ->
            ( { model
                | selectedImage = 0
                , zDoor = zDoorOpen i model.zDoor
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        selectedImage =
            model.selectedImage

        selectedImageName =
            fromInt selectedImage

        divDisplayMode =
            if selectedImage > 0 then
                "block"

            else
                "none"
    in
    div
        [ style "display" "flex"

        -- , style "opacity" "70%"
        , style "top" "0"
        , style "left" "0"
        , style "height" "800px"
        , style "width" "100%"
        , style "background-image" "url('/images/wallpaper.jpg')"
        ]
        [ ------------- GAUCHE --------------------
          div
            [ style "flex" "50" ]
            []
        , ------------- CENTRE --------------------
          div [ style "flex" "78" ]
            [ div [ style "display" "flex", style "flex-direction" "column" ]
                [ div [ style "flex" "2", style "padding" "10px", style "background-color" "#ec008c", style "color" "white" ]
                    [ text "Calendrier de l'Avent 2019 - Veepee - Media Production" ]
                , div [ style "flex" "10" ]
                    [ div
                        [ style "position" "absolute" ]
                        [ displayTable nbColumns nbLines model ]
                    , div
                        [ style "display" divDisplayMode
                        , style "position" "absolute"
                        , style "width" imgZoomWidth
                        , style "background-color" "#ec008c"
                        , style "z-index" "100"
                        ]
                        [ img
                            [ onClick (ResetSelectedImage selectedImage)
                            , style "width" imgZoomWidth
                            , src ("images/img/" ++ selectedImageName ++ ".jpg")
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , ------------- DROITE --------------------
          div [ style "flex" "50" ] []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
