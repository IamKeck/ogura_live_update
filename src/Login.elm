port module Login exposing (Model, Msg(..), init, login, loginFailed, subscriptions, update, view)

import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { mail : String
    , password : String
    , submitting : Bool
    }


type Msg
    = InputMail String
    | InputPassword String
    | Login
    | LoginFailed


port login : ( String, String ) -> Cmd msg


port loginFailed : (() -> msg) -> Sub msg


init : ( Model, Cmd Msg )
init =
    ( { mail = "", password = "", submitting = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMail s ->
            ( { model | mail = s }, Cmd.none )

        InputPassword s ->
            ( { model | password = s }, Cmd.none )

        Login ->
            if model.submitting then
                ( model, Cmd.none )

            else
                ( { model | submitting = True }
                , login ( model.mail, model.password )
                )

        LoginFailed ->
            ( { model | submitting = False }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.submitting then
        loginFailed <| always LoginFailed

    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [ class "modal", classList [ ( "is-active", model.submitting ) ] ]
            [ div [ class "modal-background" ] []
            , div [ class "modal-content loader_animation" ] [ text "送信中" ]
            ]
        , div [ class "box" ]
            [ div []
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text "email" ]
                    , div [ class "control" ]
                        [ input
                            [ onInput InputMail
                            , class "input"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "password" ]
                    , div [ class "control" ]
                        [ input
                            [ onInput InputPassword
                            , class "input"
                            , type_ "password"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div [ class "control" ]
                        [ button
                            [ onClick Login
                            , class "button is-primary"
                            ]
                            [ text "ログイン" ]
                        ]
                    ]
                ]
            ]
        ]
