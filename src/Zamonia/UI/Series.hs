{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Zamonia.UI.Series where

import           Lens.Micro           ((^.))
import           Lens.Micro.TH

import           Brick
import           Brick.Forms          (Form, editShowableField, editTextField,
                                       newForm, (@@=))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List   as L

import           Zamonia.Series
import           Zamonia.UI.Internal

makeLenses ''Series

-- | Form type
type SForm e = Form Series e Fields

-- | (Index, Status, Name)
type ListSeries = (Int, String, String)

seriesForm :: Series -> SForm e
seriesForm =
    newForm [ label "Id" @@=
                editShowableField sid Id
            , label "Title" @@=
                editTextField stitle Title (Just 1)
            , label "Original Title" @@=
                editTextField soriginalTitle OriginalTitle (Just 1)
            , label "Director" @@=
                editTextField stitle Author (Just 1)
            , label "Year" @@=
                editTextField syear Year (Just 1)
            , label "Episode Number" @@=
                editTextField epNumber EpNumber (Just 1)
            , label "Season Number" @@=
                editTextField seNumber SeNumber (Just 1)
            , label "Status" @@=
                editTextField swatched Status (Just 1)
            ]

drawSeriesList :: L.List () ListSeries -> [Widget ()]
drawSeriesList l = [ui]
    where
        label = str "[Series] - Films - Books"
        ui = joinBorders . B.borderWithLabel label $ L.renderList drawSeries True l
                <=> B.hBorder
                <=> vLimit 10 (hLimitPercent 50 (str "PLACEHOLDER" <+> fill ' ')
                    <+> B.vBorder
                    <+> hLimitPercent 50 (str "PLACEHOLDER"))

drawSeries :: Bool -> ListSeries -> Widget ()
drawSeries _ s =
    let (index, status, name) = s
     in str (show index) <+> str ": " <+> str name

{- initialSeriesList :: L.List () ListSeries
initialSeriesList = L.List () ( -}
