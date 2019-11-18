module Main exposing (..)

import Browser
import Html exposing (Html, a, div, img, table, td, text, tr)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import List exposing (filter, head, map, range)
import String exposing (fromInt)
import Task
import Time exposing (..)



---------------------------------------
----------- TYPES ---------------------


type Msg
    = NoOp
    | DisplayImage Int
    | ZoomImage Int
    | CloseImage Int
    | NewTime Time.Posix
    | Magic


type alias Model =
    { magic : Bool
    , currentDate : { zone : Time.Zone, time : Time.Posix }
    , currentCell : Cell
    , grid : Grid
    }


type alias Grid =
    List Cell


type alias Cell =
    { id : Int
    , visible : Visible
    }


type Visible
    = Door
    | Image
    | Zoom



---------------------------------------
----------- CONSTANTES ----------------


calendarHeader : String
calendarHeader =
    "Calendrier de l'Avent 2019"


calendarFooter : String
calendarFooter =
    "Veepee - Novembre 2019 - ELM AventCalendar"


maxCol : Int
maxCol =
    5


maxRow : Int
maxRow =
    5


imgHeight : Int
imgHeight =
    120


imgWidth : Int
imgWidth =
    120


borderSize : Int
borderSize =
    2


zoomWidth : Int
zoomWidth =
    maxCol * (imgWidth + 2 * borderSize) - 2 * borderSize


colorVeepee : String
colorVeepee =
    "#ec008c"


colorNotAllowed : String
colorNotAllowed =
    "Gray"



---------------------------------------
----------- INITIALISATION ------------


intialModel : Model
intialModel =
    Model
        False
        { zone = Time.utc
        , time = Time.millisToPosix 0
        }
        { id = 0
        , visible = Door
        }
        (map
            (\r ->
                { id = r
                , visible = Door
                }
            )
            (range 1 (maxCol * maxRow))
        )


init : ( Model, Cmd Msg )
init =
    ( intialModel
    , getNewTime
    )


getNewTime : Cmd Msg
getNewTime =
    Task.perform NewTime Time.now



---------------------------------------
----------- HELPERS -------------------


find : Int -> Grid -> Cell
find i grid =
    case
        head <|
            filter
                (\b ->
                    b.id == i
                )
                grid
    of
        Just cell ->
            cell

        Nothing ->
            { id = 0, visible = Door }


toId : Int -> Int -> Int
toId col row =
    col * maxCol + row - maxCol


setCell : Int -> Visible -> Grid -> Grid
setCell id visible grid =
    map
        (\cell ->
            { cell
                | visible =
                    if cell.id == id then
                        visible

                    else if visible == Image then
                        Door

                    else
                        cell.visible
            }
        )
        grid


toPx : Int -> String
toPx i =
    fromInt i ++ "px"



---------------------------------------
----------- HTML ----------------------


displayGrid : Int -> Int -> Model -> Visible -> Html Msg
displayGrid column line model visible =
    let
        gridDisplayMode =
            if visible == Zoom then
                "none"

            else
                "block"
    in
    table
        [ style "display" gridDisplayMode
        ]
        (displayTr column line model)


displayTr : Int -> Int -> Model -> List (Html Msg)
displayTr column line model =
    map
        (\r ->
            tr []
                (displayTd column line r model)
        )
        (range 1 line)


displayTd : Int -> Int -> Int -> Model -> List (Html Msg)
displayTd column line indiceColumn model =
    map
        (\r ->
            td []
                [ let
                    i =
                        toId indiceColumn r

                    cell =
                        find i model.grid

                    date =
                        model.currentDate

                    zone =
                        date.zone

                    time =
                        date.time

                    magic =
                        model.magic
                  in
                  div []
                    [ displayCell cell (Time.toDay zone time) magic
                    ]
                ]
        )
        (range 1 column)


displayCell : Cell -> Int -> Bool -> Html Msg
displayCell b nowDay magic =
    if b.visible == Door then
        displayDoor b.id nowDay magic

    else
        displayImage b.id


displayImage : Int -> Html Msg
displayImage i =
    div
        [ onClick (ZoomImage i)
        , style "cursor" "pointer"
        , style "width" (toPx imgWidth)
        , style "height" (toPx imgHeight)
        , style "background-color" "WHITE"
        ]
        [ img
            [ style "height" (toPx imgHeight)
            , src ("images/img/" ++ fromInt i ++ ".jpg")
            ]
            []
        ]


displayDoor : Int -> Int -> Bool -> Html Msg
displayDoor i nowDay magic =
    let
        cursorPointer =
            if i <= nowDay || magic then
                "pointer"

            else
                "not-allowed"

        colorDay =
            if i <= nowDay || magic then
                colorVeepee

            else
                colorNotAllowed
    in
    div
        [ style "cursor" cursorPointer
        , if i <= nowDay || magic then
            onClick (DisplayImage i)

          else
            onClick NoOp
        , style "width" (toPx imgWidth)
        , style "height" (toPx imgHeight)
        ]
        [ div [ style "position" "absolute" ]
            [ img
                [ style "height" (toPx imgHeight)
                , src ("images/door/door" ++ fromInt i ++ ".jpg")
                ]
                []
            ]
        , div
            [ style "position" "relative"
            , style "padding" "20px"
            , style "color" colorDay
            , style "font-size" "50px"
            , style "text-shadow" "white -1px 0px, white 0px -1px, white 2px 0px, white 0px 2px"
            ]
            [ text (fromInt i)
            ]
        ]


displayZoom : Int -> Visible -> Html Msg
displayZoom id visible =
    let
        zoomDisplayMode =
            case visible of
                Zoom ->
                    "block"

                _ ->
                    "none"
    in
    table
        [ style "display" zoomDisplayMode
        ]
        [ tr []
            [ td []
                [ img
                    [ style "cursor" "pointer"
                    , onClick (CloseImage id)
                    , src ("images/img/" ++ fromInt id ++ ".jpg")
                    , style "width" (toPx zoomWidth)
                    , style "background-color" colorVeepee
                    ]
                    []
                ]
            ]
        ]


displayHeader : Html Msg
displayHeader =
    div
        [ style "padding" "10px"
        , style "background-color" colorVeepee
        , style "color" "white"
        ]
        [ text calendarHeader
        ]


displayFooter : Html Msg
displayFooter =
    div
        [ style "padding" "8px"
        , style "background-color" colorVeepee
        , style "color" "white"
        ]
        [ text calendarFooter
        , a
            [ style "cursor" "pointer"
            , onClick Magic
            ]
            [ text "2019"
            ]
        ]



---------------------------------------
----------- VIEW ----------------------


view : Model -> Html Msg
view model =
    let
        cell =
            model.currentCell

        id =
            cell.id

        visible =
            cell.visible

        date =
            model.currentDate

        zone =
            date.zone

        time =
            date.time
    in
    div
        [ style "display" "flex"
        , style "height" "1440px"
        , style "width" "100%"
        , style "background-image" "url('/images/wallpaper.jpg')"
        ]
        [ ------------- GAUCHE --------------------
          div
            [ style "flex" "50"
            ]
            []
        , ------------- CENTRE --------------------
          div [ style "flex" "78" ]
            [ div
                [ style "margin-top" "10px"
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ div
                    [ style "flex" "2"
                    ]
                    [ displayHeader
                    ]
                , div
                    [ style "flex" "8"
                    ]
                    [ displayGrid maxCol maxRow model visible
                    , displayZoom id visible
                    ]
                , div
                    [ style "flex" "2"
                    ]
                    [ displayFooter
                    ]
                ]
            ]
        , ------------- DROITE --------------------
          div [ style "flex" "50" ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---------------------------------------
----------- UPDATE --------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayImage i ->
            ( { model
                | currentCell =
                    { id = i
                    , visible = Image
                    }
                , grid = setCell i Image model.grid
              }
            , Cmd.none
            )

        ZoomImage i ->
            let
                cell =
                    { id = i
                    , visible = Zoom
                    }
            in
            ( { model
                | currentCell = cell
                , grid = setCell i Zoom model.grid
              }
            , Cmd.none
            )

        CloseImage i ->
            let
                cell =
                    { id = i
                    , visible = Image
                    }
            in
            ( { model
                | currentCell = cell
                , grid = setCell i Image model.grid
              }
            , Cmd.none
            )

        NewTime newTime ->
            let
                date =
                    model.currentDate

                new =
                    { date
                        | time = newTime
                    }
            in
            ( { model
                | currentDate = new
              }
            , Cmd.none
            )

        Magic ->
            ( { model
                | magic = not model.magic
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---------------------------------------
----------- PROGRAMME -----------------


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
