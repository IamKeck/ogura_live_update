port module Main exposing (Flag, Model, Msg, main)

import Browser
import Html exposing (Html, button, div, h1, pre, section, text, textarea)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Login
import Random


type Model
    = LoggedIn
        { currentStaging : String
        , currentProduction : String
        , inputStaging : String
        , inputProduction : String
        , currentPage : ValueType
        , submitting : Bool
        , textareaKey : TextareaKey
        }
    | NotLoggedIn Login.Model


type ValueType
    = Staging
    | Production


type Msg
    = LoggedInMsg LoggedInMsg
    | NotLoggedInMsg Login.Msg
    | LoginStatusChanged Bool


type LoggedInMsg
    = InputStaging String
    | InputProduction String
    | SubmitStaging
    | SubmitProduction
    | UpdateCurrentStatus DbData
    | SubmitDone
    | SwitchPage
    | GotKey TextareaKey
    | Logout


type alias Flag =
    Bool


type alias TextareaKey =
    { staging : String, production : String }


type alias DbData =
    { staging : String, production : String }


port listenDb : (DbData -> msg) -> Sub msg


port listenLoginStatus : (Bool -> msg) -> Sub msg


port submitStaging : String -> Cmd msg


port submitProduction : String -> Cmd msg


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
            , textareaKey = TextareaKey "" ""
            }
        , Random.generate (GotKey >> LoggedInMsg) <| generateTextareaKey (TextareaKey "" "")
        )

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


generateTextareaKey : TextareaKey -> Random.Generator TextareaKey
generateTextareaKey oldKey =
    let
        keyGen =
            Random.map String.fromInt <| Random.int Random.minInt Random.maxInt
    in
    Random.map2 TextareaKey keyGen keyGen
        |> Random.andThen
            (\newKey ->
                if newKey.staging == oldKey.staging || newKey.production == oldKey.production then
                    generateTextareaKey oldKey

                else
                    Random.constant newKey
            )


subscriptions : Model -> Sub Msg
subscriptions m =
    let
        additionalSub =
            case m of
                NotLoggedIn m2 ->
                    Sub.map NotLoggedInMsg <| Login.subscriptions m2

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
                                            , Random.generate (GotKey >> LoggedInMsg) <|
                                                generateTextareaKey model2.textareaKey
                                            )

                                        Production ->
                                            ( LoggedIn
                                                { model2
                                                    | submitting = False
                                                    , inputProduction = ""
                                                }
                                            , Random.generate (GotKey >> LoggedInMsg) <|
                                                generateTextareaKey model2.textareaKey
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

                        GotKey key ->
                            ( LoggedIn { model2 | textareaKey = key }, Cmd.none )

                        Logout ->
                            ( model, logout () )

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
                    let
                        subtitle =
                            case m2.currentPage of
                                Staging ->
                                    "確認用環境(Staging)"

                                Production ->
                                    "本公開環境(Production)"

                        currentArticle =
                            case m2.currentPage of
                                Staging ->
                                    m2.currentStaging

                                Production ->
                                    m2.currentProduction

                        onInputEvent =
                            case m2.currentPage of
                                Staging ->
                                    InputStaging

                                Production ->
                                    InputProduction

                        submitEvent =
                            case m2.currentPage of
                                Staging ->
                                    SubmitStaging

                                Production ->
                                    SubmitProduction

                        textareaKey =
                            case m2.currentPage of
                                Staging ->
                                    m2.textareaKey.staging

                                Production ->
                                    m2.textareaKey.production

                        currentInput =
                            case m2.currentPage of
                                Staging ->
                                    m2.inputStaging

                                Production ->
                                    m2.inputProduction
                    in
                    Html.map LoggedInMsg <|
                        div []
                            [ div
                                []
                                [ div [ class "modal", classList [ ( "is-active", m2.submitting ) ] ]
                                    [ div [ class "modal-background" ] []
                                    , div [ class "modal-content loader_animation" ] [ text "送信中" ]
                                    ]
                                , div [ class "columns" ]
                                    [ div [ class "box column" ]
                                        [ h1 [ class "subtitle is-3" ]
                                            [ text subtitle ]
                                        , pre [ class "current_article" ]
                                            [ text currentArticle ]
                                        ]
                                    , div [ class "box column" ]
                                        [ div [ class "switch_button_wrapper" ]
                                            [ button [ onClick SwitchPage, class "button" ]
                                                [ text "切替" ]
                                            ]
                                        , Html.Keyed.node "div"
                                            []
                                            [ ( textareaKey
                                              , textarea
                                                    [ class "article_textarea"
                                                    , onInput onInputEvent
                                                    ]
                                                    [ text currentInput
                                                    ]
                                              )
                                            ]
                                        , div [ class "" ]
                                            [ button
                                                [ onClick submitEvent
                                                , class "button is-success"
                                                ]
                                                [ text "送信"
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div []
                                [ button [ onClick Logout, class "button is-danger" ] [ text "ログアウト" ]
                                ]
                            ]

                NotLoggedIn m2 ->
                    Html.map NotLoggedInMsg <| Login.view m2
    in
    div [ class "container" ]
        [ div [ class "header" ] [ h1 [ class "title is-2" ] [ text "記事管理ページ" ] ]
        , section [ class "section" ] [ content ]
        ]
