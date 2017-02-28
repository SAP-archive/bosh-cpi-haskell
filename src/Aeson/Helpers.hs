module Aeson.Helpers(
    fieldLabelMap
) where

fieldLabelMap :: [(String, String)] -> String -> String
fieldLabelMap map _label = let
  label = drop 1 _label
  in case lookup label map of
      Just replacement -> replacement
      Nothing          -> label
