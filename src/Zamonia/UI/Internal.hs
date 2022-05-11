{-# LANGUAGE ScopedTypeVariables #-}
module Zamonia.UI.Internal where

import           Data.Functor   ((<&>))
import           Data.Maybe     (fromMaybe)
import           Data.Proxy
import qualified Data.Text      as T
import           Lens.Micro     ((%~), (&))
import           Lens.Micro.TH
import           Zamonia.Series
import           Zamonia.Work   (Work, new)

import           Brick
import           Brick.Focus          (focusGetCurrent, focusRingCursor)
import           Brick.Forms          (Form, focusedFormInputAttr, formFocus,
                                       formState, handleFormEvent,
                                       invalidFields, invalidFormInputAttr,
                                       renderForm)
-- import qualified Brick.Main           as M
import qualified Brick.Types          as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E
import qualified Graphics.Vty         as V

data Fields = Id -- ^ ID or ISBN
            | Author -- ^ Author or Dirctor
            | EpNumber -- ^ Number of episodes
            | OriginalTitle -- ^ Original Title
            | Possession -- ^ Possession
            | Publisher -- ^ Publisher
            | SeNumber -- ^ Number of seasons
            | Status -- ^ Status
            | Title -- ^ Title
            | Year -- ^ Year of release
    deriving (Eq, Ord, Show)

label :: String -> Widget n -> Widget n
label s w = padTop (Pad 1) (vLimit 1 (hLimit 15 $ str s <+> fill ' ')) <+> B.border w

drawForm :: Work w => Form w e Fields -> [Widget Fields]
drawForm f = [C.vCenter $ C.hCenter form]
    where
        form = B.borderWithLabel (str "Edit") $ padTop (Pad 1) $ hLimit 50 $ renderForm f

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.white `on` V.black)
  , (invalidFormInputAttr, V.brightRed `on` V.black)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]
