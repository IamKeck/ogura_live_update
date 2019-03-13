port module Main exposing (Flag, Model, Msg, main)

import Browser
import Html exposing (Html, button, div, h1, input, label, pre, section, table, td, text, textarea, th, tr)
import Html.Attributes exposing (class, id, type_)
import Html.Events exposing (onClick, onInput)


type Model
    = LoggedIn
        { currentStaging : String
        , currentProduction : String
        , inputStaging : String
        , inputProduction : String
        , submitting : Maybe ValueType
        }
    | NotLoggedIn
        { mail : String
        , password : String
        }


type ValueType
    = Staging
    | Production


type Msg
    = LoggedInMsg LoggedInMsg
    | NotLoggedInMsg NotLoggedInMsg
    | LoginStatusChanged Bool


type LoggedInMsg
    = InputStaging String
    | InputProduction String
    | SubmitStaging
    | SubmitProduction
    | UpdateCurrentStatus DbData
    | SubmitDone
    | Logout


type NotLoggedInMsg
    = InputMail String
    | InputPassword String
    | Login


type alias Flag =
    Bool


type alias DbData =
    { staging : String, production : String }


port listenDb : (DbData -> msg) -> Sub msg


port listenLoginStatus : (Bool -> msg) -> Sub msg


port submitStaging : String -> Cmd msg


port submitProduction : String -> Cmd msg


port login : ( String, String ) -> Cmd msg


port logout : () -> Cmd msg


port submitDone : (() -> msg) -> Sub msg


init : Flag -> ( Model, Cmd Msg )
init isLoggedIn =
    if isLoggedIn then
        ( LoggedIn
            { currentStaging = ""
            , currentProduction = ""
            , inputStaging = ""
            , inputProduction = ""
            , submitting = Nothing
            }
        , Cmd.none
        )

    else
        ( NotLoggedIn { mail = "", password = "" }, Cmd.none )


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
                NotLoggedIn _ ->
                    Sub.none

                LoggedIn m2 ->
                    Sub.batch
                        [ listenDb (UpdateCurrentStatus >> LoggedInMsg)
                        , case m2.submitting of
                            Just _ ->
                                submitDone <| always (LoggedInMsg SubmitDone)

                            Nothing ->
                                Sub.none
                        ]

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
                    case msg2 of
                        InputStaging s ->
                            ( LoggedIn { model2 | inputStaging = s }, Cmd.none )

                        InputProduction s ->
                            ( LoggedIn { model2 | inputProduction = s }, Cmd.none )

                        SubmitStaging ->
                            ( LoggedIn { model2 | submitting = Just Staging }
                            , submitStaging model2.inputStaging
                            )

                        SubmitProduction ->
                            ( LoggedIn { model2 | submitting = Just Production }
                            , submitProduction model2.inputProduction
                            )

                        UpdateCurrentStatus s ->
                            Debug.log (Debug.toString s) <|
                                ( LoggedIn
                                    { model2
                                        | currentStaging = s.staging
                                        , currentProduction = s.production
                                    }
                                , Cmd.none
                                )

                        SubmitDone ->
                            case model2.submitting of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just Staging ->
                                    ( LoggedIn
                                        { model2
                                            | submitting = Nothing
                                        }
                                    , Cmd.none
                                    )

                                Just Production ->
                                    ( LoggedIn
                                        { model2
                                            | submitting = Nothing
                                        }
                                    , Cmd.none
                                    )

                        Logout ->
                            ( model, logout () )

                _ ->
                    ( model, Cmd.none )

        NotLoggedInMsg msg2 ->
            case model of
                NotLoggedIn model2 ->
                    case msg2 of
                        InputMail s ->
                            ( NotLoggedIn { model2 | mail = s }, Cmd.none )

                        InputPassword s ->
                            ( NotLoggedIn { model2 | password = s }, Cmd.none )

                        Login ->
                            ( model, login ( model2.mail, model2.password ) )

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
                    Html.map LoggedInMsg <|
                        div []
                            [ div
                                [ class "columns" ]
                                [ case m2.submitting of
                                    Just _ ->
                                        div [ class "" ] [ text "送信中" ]

                                    Nothing ->
                                        div [] []
                                , div [ class "box column" ]
                                    [ h1 [ class "subtitle is-3" ] [ text "Staging" ]
                                    , pre [ class "current_article" ] [ text m2.currentStaging ]
                                    , div [ class "" ] [ textarea [ onInput InputStaging, id "input_staging" ] [] ]
                                    , div [ class "" ] [ button [ onClick SubmitStaging ] [ text "submit staging" ] ]
                                    ]
                                , div [ class "box column" ]
                                    [ h1 [ class "subtitle is-3" ] [ text "Production" ]
                                    , pre [ class "current_article" ] [ text m2.currentProduction ]
                                    , div [ class "" ] [ textarea [ onInput InputProduction, id "input_production" ] [] ]
                                    , div [ class "" ] [ button [ onClick SubmitProduction ] [ text "submit production" ] ]
                                    ]
                                ]
                            , div []
                                [ button [ onClick Logout, class "button is-danger" ] [ text "ログアウト" ]
                                ]
                            ]

                NotLoggedIn _ ->
                    Html.map NotLoggedInMsg <|
                        div []
                            [ div [ class "box" ]
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
    in
    div [ class "container" ]
        [ div [ class "header" ] [ h1 [ class "title is-2" ] [ text "記事管理ページ" ] ]
        , section [ class "section" ] [ content ]
        ]
