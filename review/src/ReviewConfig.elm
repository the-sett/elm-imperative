module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import ReviewPipelineStyles as Pipeline
import ReviewPipelineStyles.Premade as Premade
import ReviewPipelineStyles.Fixes as Fixes
import ReviewPipelineStyles.Predicates as Predicates


config : List Rule
config =
    [ Pipeline.rule <|
        List.concat
            [ Premade.noMultilineLeftPizza
            , Premade.noMultilineLeftComposition
            , Premade.noSemanticallyInfixFunctionsInLeftPipelines
            , [ Pipeline.forbid Pipeline.leftPizzaPipelines
                |> Pipeline.andTryToFixThemBy Fixes.convertingToRightPizza
                |> Pipeline.andCallThem "<| pipeline with several steps" ]
            , [ Pipeline.forbid Pipeline.leftCompositionPipelines
                |> Pipeline.andTryToFixThemBy Fixes.convertingToRightComposition
                |> Pipeline.andCallThem "<| pipeline with several steps" ]
            ]
    ]