module Array2D.Json exposing (decoder)

{-| JSON decoding/encoding utilities for Array2D.

# Decoding 2D arrays
@docs decoder
-}

import Array2D exposing (Array2D)
import Json.Decode as Json
import Array2D.ArrayHelpers as Helpers


{-| JSON decoder to turn nested arrays into an Array2D.

If the rows are jagged, all rows will be truncated to the
same length as the shortest row!
-}
decoder : Json.Decoder a -> Json.Decoder (Array2D a)
decoder cellDecoder =
    (Json.array (Json.array cellDecoder))
        |> Json.andThen
            (\array ->
                let
                    ( columns, normalizedData ) =
                        Helpers.getMinColumnsAndTruncateRows array
                in
                    Json.succeed { data = normalizedData, columns = columns }
            )
