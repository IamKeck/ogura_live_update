port module Main exposing (Flag, Model, Msg, main)

import Browser
import Html exposing (Html, button, div, h1, input, label, pre, section, table, td, text, textarea, th, tr)
import Html.Attributes exposing (class, id, type_)
import Html.Events exposing (onClick, onInput)
import Html.Keyed


type Model
    = LoggedIn
        { currentStaging : String
        , currentProduction : String
        , inputStaging : String
        , inputProduction : String
        , currentPage : ValueType
        , submitting : Bool
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
    | SwitchPage
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
            , currentPage = Staging
            , submitting = False
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
                            True ->
                                submitDone <| always (LoggedInMsg SubmitDone)

                            False ->
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
                            ( LoggedIn { model2 | submitting = True }
                            , if model2.submitting then
                                Cmd.none

                              else
                                submitStaging model2.inputStaging
                            )

                        SubmitProduction ->
                            ( LoggedIn { model2 | submitting = True }
                            , if model2.submitting then
                                Cmd.none

                              else
                                submitProduction model2.inputProduction
                            )

                        UpdateCurrentStatus s ->
                            ( LoggedIn
                                { model2
                                    | currentStaging = s.staging
                                    , currentProduction = s.production
                                }
                            , Cmd.none
                            )

                        SubmitDone ->
                            case model2.submitting of
                                False ->
                                    ( model, Cmd.none )

                                True ->
                                    case model2.currentPage of
                                        Staging ->
                                            ( LoggedIn
                                                { model2
                                                    | submitting = False
                                                    , inputStaging = ""
                                                }
                                            , Cmd.none
                                            )

                                        Production ->
                                            ( LoggedIn
                                                { model2
                                                    | submitting = False
                                                    , inputProduction = ""
                                                }
                                            , Cmd.none
                                            )

                        SwitchPage ->
                            if model2.submitting then
                                ( model, Cmd.none )

                            else
                                let
                                    newPage =
                                        case model2.currentPage of
                                            Staging ->
                                                Production

                                            Production ->
                                                Staging
                                in
                                ( LoggedIn { model2 | currentPage = newPage }, Cmd.none )

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
                                []
                                [ case m2.submitting of
                                    True ->
                                        div [ class "" ] [ text "送信中" ]

                                    False ->
                                        div [] []
                                , div [ class "columns" ]
                                    [ div [ class "box column" ]
                                        [ h1 [ class "subtitle is-3" ]
                                            [ text <|
                                                case m2.currentPage of
                                                    Staging ->
                                                        "Staging"

                                                    Production ->
                                                        "Production"
                                            ]
                                        , pre [ class "current_article" ]
                                            [ text <|
                                                case m2.currentPage of
                                                    Staging ->
                                                        m2.currentStaging

                                                    Production ->
                                                        m2.currentProduction
                                            ]
                                        ]
                                    , div [ class "box column" ]
                                        [ div [ class "switch_button_wrapper" ]
                                            [ button [ onClick SwitchPage, class "button" ]
                                                [ text "switch" ]
                                            ]
                                        , Html.Keyed.node "div"
                                            []
                                            [ ( case m2.currentPage of
                                                    Production ->
                                                        "p"

                                                    Staging ->
                                                        "s"
                                              , textarea
                                                    [ class "article_textarea"
                                                    , onInput <|
                                                        case m2.currentPage of
                                                            Staging ->
                                                                InputStaging

                                                            Production ->
                                                                InputProduction
                                                    , id <|
                                                        case m2.currentPage of
                                                            Staging ->
                                                                "input_staging"

                                                            Production ->
                                                                "input_production"
                                                    ]
                                                    [ text <|
                                                        case m2.currentPage of
                                                            Staging ->
                                                                m2.inputStaging

                                                            Production ->
                                                                m2.inputProduction
                                                    ]
                                              )
                                            ]
                                        , div [ class "" ]
                                            [ button
                                                [ onClick <|
                                                    case m2.currentPage of
                                                        Staging ->
                                                            SubmitStaging

                                                        Production ->
                                                            SubmitProduction
                                                , class "button is-success"
                                                ]
                                                [ text <|
                                                    case m2.currentPage of
                                                        Staging ->
                                                            "submit staging"

                                                        Production ->
                                                            "submit production"
                                                ]
                                            ]
                                        ]
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
