{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module HGit.GUI.CSS where

import Clay

css :: Css
css = do
    button ? do
      backgroundColor linen
      borderStyle solid
      borderWidth (px 2)
      borderColor black
      display inlineBlock
      color black
      ".focus-small" & do
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)

      ".expand-small" & do
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)

      ".minimize-small" & do
        borderRadius (pct 50) (pct 50) (pct 50) (pct 50)


    legend ? ".vertical-legend" & do
      backgroundColor none
      borderRight solid (px 4) linen
      color black
      textAlign center
      padding (px 3) (px 3) (px 3) (px 3)
      height (pct 100)
      position absolute
      left (unitless 0)
      top  (unitless 0)

      -- /* turn the text sideways */
      "-o-writing-mode" -: "vertical-lr"
      "-ms-writing-mode" -: "vertical-lr"
      "-moz-writing-mode" -: "vertical-lr"
      "-webkit-writing-mode" -: "vertical-lr"
      "writing-mode" -: "vertical-lr"

      -- /* flip th etext around */
      "-o-transform" -: "rotate(180deg)"      -- /* Opera */
      "-ms-transform" -: "rotate(180deg)"     -- /* IE */
      "-moz-transform" -: "rotate(180deg)"    -- /* Moz */
      "-webkit-transform" -: "rotate(180deg)" -- /* Safari */
      "transform" -: "rotate(180deg0)"

    fieldset ? ".vertical-legend" & do
      minHeight (px 150)
      position relative
      borderStyle none

    fieldset ? ".node" & do
      borderColor linen
      borderWidth (px 4)
      borderRadius (px 3) (px 3) (px 3) (px 3)
      ".snapshot" & do
        backgroundColor salmon
      ".filetree" & do
        backgroundColor skyblue
      ".commit" & do
        backgroundColor violet
      ".blob" & do
        backgroundColor turquoise
      ".wip" & do
        borderStyle dashed
      ".persisted" & do
        borderStyle solid
    legend ? do
      borderWidth (px 4)
      backgroundColor inherit
      padding (px 5) (px 10) (px 5) (px 5)
      borderColor linen
      borderStyle inherit
      borderRadius (px 3) (px 3) (px 3) (px 3)
