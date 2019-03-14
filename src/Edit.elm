port module Edit exposing (DbData, Model, Msg(..), TextareaKey, ValueType(..), generateTextareaKey, init, listenDb, logout, submitDone, submitProduction, submitStaging, subscriptions, update, view)

import Html exposing (Html, button, div, h1, pre, text, textarea)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Random


type alias Model =
    { currentStaging : String
    , currentProduction : String
    , inputStaging : String
    , inputProduction : String
    , currentPage : ValueType
    , submitting : Bool
    , textareaKey : TextareaKey
    }


type Msg
    = InputStaging String
    | InputProduction String
    | SubmitStaging
    | SubmitProduction
    | UpdateCurrentStatus DbData
    | SubmitDone
    | SwitchPage
    | GotKey TextareaKey
    | Logout


type ValueType
    = Staging
    | Production


type alias TextareaKey =
    { staging : String, production : String }


type alias DbData =
    { staging : String, production : String }


port listenDb : (DbData -> msg) -> Sub msg


port submitStaging : String -> Cmd msg


port submitProduction : String -> Cmd msg


port logout : () -> Cmd msg


port submitDone : (() -> msg) -> Sub msg


init : ( Model, Cmd Msg )
init =
    ( { currentStaging = ""
      , currentProduction = ""
      , inputStaging = ""
      , inputProduction = ""
      , currentPage = Staging
      , submitting = False
      , textareaKey = TextareaKey "" ""
      }
    , Random.generate GotKey <| generateTextareaKey (TextareaKey "" "")
    )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ listenDb UpdateCurrentStatus
        , case m.submitting of
            True ->
                submitDone <| always SubmitDone

            False ->
                Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputStaging s ->
            ( { model | inputStaging = s }, Cmd.none )

        InputProduction s ->
            ( { model | inputProduction = s }, Cmd.none )

        SubmitStaging ->
            ( { model | submitting = True }
            , if model.submitting then
                Cmd.none

              else
                submitStaging model.inputStaging
            )

        SubmitProduction ->
            ( { model | submitting = True }
            , if model.submitting then
                Cmd.none

              else
                submitProduction model.inputProduction
            )

        UpdateCurrentStatus s ->
            ( { model
                | currentStaging = s.staging
                , currentProduction = s.production
              }
            , Cmd.none
            )

        SubmitDone ->
            case model.submitting of
                False ->
                    ( model, Cmd.none )

                True ->
                    case model.currentPage of
                        Staging ->
                            ( { model
                                | submitting = False
                                , inputStaging = ""
                              }
                            , Random.generate GotKey <|
                                generateTextareaKey model.textareaKey
                            )

                        Production ->
                            ( { model
                                | submitting = False
                                , inputProduction = ""
                              }
                            , Random.generate GotKey <|
                                generateTextareaKey model.textareaKey
                            )

        SwitchPage ->
            if model.submitting then
                ( model, Cmd.none )

            else
                let
                    newPage =
                        case model.currentPage of
                            Staging ->
                                Production

                            Production ->
                                Staging
                in
                ( { model | currentPage = newPage }, Cmd.none )

        GotKey key ->
            ( { model | textareaKey = key }, Cmd.none )

        Logout ->
            ( model, logout () )


view : Model -> Html Msg
view model =
    let
        subtitle =
            case model.currentPage of
                Staging ->
                    "確認用環境(Staging)"

                Production ->
                    "本公開環境(Production)"

        currentArticle =
            case model.currentPage of
                Staging ->
                    model.currentStaging

                Production ->
                    model.currentProduction

        onInputEvent =
            case model.currentPage of
                Staging ->
                    InputStaging

                Production ->
                    InputProduction

        submitEvent =
            case model.currentPage of
                Staging ->
                    SubmitStaging

                Production ->
                    SubmitProduction

        textareaKey =
            case model.currentPage of
                Staging ->
                    model.textareaKey.staging

                Production ->
                    model.textareaKey.production

        currentInput =
            case model.currentPage of
                Staging ->
                    model.inputStaging

                Production ->
                    model.inputProduction
    in
    div []
        [ div
            []
            [ div [ class "modal", classList [ ( "is-active", model.submitting ) ] ]
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
