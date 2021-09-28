{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Merkle.GUI.CSS where

import Prelude hiding (span, div)
import Clay




css :: Css
css = do
    ul ? do
      listStyleType none
      marginLeft  (unitless 0)
      paddingLeft (unitless 0)


    span ? do
      ":hover" |> ".dropdown" ? do
        display block

    div ? do
      ".sidebar" & do
        backgroundColor gold
        borderStyle solid

      borderColor black
      borderWidth (px 3)
      ".wip" & do
        borderStyle dashed
      ".persisted" & do
        borderStyle solid

      ".snapshot" & do
        backgroundColor palegreen
      ".filetree" & do
        backgroundColor aqua
      ".commit" & do
        backgroundColor hotpink
      ".blob" & do
        backgroundColor tomato

      ".branch-browser" & do
        borderStyle solid
        backgroundColor paleturquoise

      ".infra" & do
        margin (px 15) (px 15) (px 15) (px 15)

    div ? do
      ".modal" & do
          position fixed
          zIndex 9999
          left (unitless 0)
          top (unitless 0)
          width (pct 100)
          height (pct 100)
          overflow auto
          backgroundColor $ rgba 0 0 0 0.4

    div ? do
      ".modal-content" & do
          margin (pct 15) (pct 15) (pct 15) (pct 15)
          padding (px 20) (px 20) (px 20) (px 20)
          width auto

      ".popup-content" & do
          margin (px 20) (px 20) (px 20) (px 20)

    div ? do
      ".dropdown" & do
        display none
        backgroundColor inherit
        borderColor black
        borderStyle solid
        position absolute
        zIndex 999
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)

        alignContent center
        left         (em (-0.5))

        button ? do
          backgroundColor linen
          borderStyle solid
          borderColor black
          textAlign center
          textDecoration none
          cursor pointer
          display block


    body ? do
      backgroundImage $ url "/static/vaporwave.jpg"

    li ? do
      marginTop (em 1)

    ".hidden" & do
      display none

    Clay.span ? do
      ".clickable-bullet" & do

        padding (em 0) (em 0) (em 0) (em 1)
        left (em (-4))
        borderStyle solid
        borderWidth (px 2)
        borderColor black


        ".branch" & do
          color black
          backgroundColor turquoise
          ".focus" & do
            backgroundColor teal
          color white

        ".add" & do
          color white
          backgroundColor limegreen

        ".del" & do
          color white
          backgroundColor red

        ".snapshot" & do
          backgroundColor palegreen
          ":hover" & do
            backgroundColor paleturquoise
        ".filetree" & do
          backgroundColor aqua
          ":hover" & do
            backgroundColor skyblue
        ".commit" & do
          backgroundColor hotpink
          ":hover"  & do
            backgroundColor pink
        ".blob" & do
          backgroundColor tomato
          ":hover" & do
            backgroundColor salmon

