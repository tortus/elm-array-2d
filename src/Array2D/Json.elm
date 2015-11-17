module Array2D.Json (decoder) where

{-| JSON decoding/encoding utilities for Array2D. -}

import Array2D exposing (Array2D)
import Json.Decode as Json


{-| JSON decoder to turn nested arrays into an Array2D.

Be sure they are NOT jagged. Normalize first.
-}
decoder : Json.Decoder a -> Json.Decoder (Array2D a)
decoder cellDecoder =
  Json.object1
    Array2D
    (Json.array (Json.array cellDecoder))
