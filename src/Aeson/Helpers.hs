module Aeson.Helpers(
    fieldLabelMap
) where

fieldLabelMap :: [(String, String)] -> String -> String
fieldLabelMap map label = let
  in case lookup label map of
      Just replacement -> replacement
      Nothing          -> label
