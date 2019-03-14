port module Main exposing (Flag, Model, Msg, main)

import Browser
import Edit
import Html exposing (Html, div, h1, section, text)
import Html.Attributes exposing (class)
import Login


type Model
    = LoggedIn Edit.Model
    | NotLoggedIn Login.Model


type Msg
    = LoggedInMsg Edit.Msg
    | NotLoggedInMsg Login.Msg
    | LoginStatusChanged Bool


type alias Flag =
    Bool


port listenLoginStatus : (Bool -> msg) -> Sub msg


init : Flag -> ( Model, Cmd Msg )
init isLoggedIn =
    if isLoggedIn then
        let
            ( model, cmd ) =
                Edit.init
        in
        ( LoggedIn model, Cmd.map LoggedInMsg cmd )

    else
        let
            ( model, cmd ) =
                Login.init
        in
        ( NotLoggedIn model, Cmd.map NotLoggedInMsg cmd )


main : Program Flag Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions m =
    let
        additionalSub =
            case m of
                NotLoggedIn m2 ->
                    Sub.map NotLoggedInMsg <| Login.subscriptions m2

                LoggedIn m2 ->
                    Sub.map LoggedInMsg <| Edit.subscriptions m2

        listenLoginStat =
            listenLoginStatus LoginStatusChanged
    in
    Sub.batch [ listenLoginStat, additionalSub ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoggedInMsg msg2 ->
            case model of
                LoggedIn model2 ->
                    let
                        ( model3, cmd ) =
                            Edit.update msg2 model2
                    in
                    ( LoggedIn model3, Cmd.map LoggedInMsg cmd )

                _ ->
                    ( model, Cmd.none )

        NotLoggedInMsg msg2 ->
            case model of
                NotLoggedIn model2 ->
                    let
                        ( model3, cmd ) =
                            Login.update msg2 model2
                    in
                    ( NotLoggedIn model3, Cmd.map NotLoggedInMsg cmd )

                _ ->
                    ( model, Cmd.none )

        LoginStatusChanged isLoggedIn ->
            init isLoggedIn


view : Model -> Html Msg
view m =
    let
        content =
            case m of
                LoggedIn m2 ->
                    Html.map LoggedInMsg <| Edit.view m2

                NotLoggedIn m2 ->
                    Html.map NotLoggedInMsg <| Login.view m2
    in
    div [ class "container" ]
        [ div [ class "header" ] [ h1 [ class "title is-2" ] [ text "記事管理ページ" ] ]
        , section [ class "section" ] [ content ]
        ]
