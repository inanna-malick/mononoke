{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module HGit.GUI.CSS where

import Prelude hiding (span, div)
import Clay

css :: Css
css = do
    ul ? do
      listStyleType none
      marginLeft  (unitless 0)
      paddingLeft (unitless 0)


    span ? do
      ":hover" |> ".vis-on-hover" ? do
        display block

    div ? do
      ".dropdown" & do
        display none
        backgroundColor inherit
        borderColor black
        borderStyle solid
        position absolute
        zIndex 999
        padding (em 0) (em 0) (em 0) (em 1)
        left (em (-4))

        button ? do
          backgroundColor linen
          borderStyle solid
          borderColor black
          textAlign center
          textDecoration none
          cursor pointer
          display block

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

        ".snapshot" & do
          backgroundColor palegreen
          ":hover" & do
            backgroundColor paleturquoise
        ".filetree" & do
          backgroundColor aqua
          ":hover" & do
            backgroundColor skyblue
        ".commit" & do
          backgroundColor magenta
          ":hover"  & do
            backgroundColor hotpink
        ".blob" & do
          backgroundColor tomato
          ":hover" & do
            backgroundColor salmon

