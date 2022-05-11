{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Zamonia.UI.App where

import           Data.Functor   ((<&>))
import           Data.Maybe     (fromMaybe)
import           Data.Proxy
import qualified Data.Text      as T
import           Lens.Micro     ((%~), (&))
import           Lens.Micro.TH

import           Brick
import           Brick.Focus          (focusGetCurrent, focusRingCursor)
import           Brick.Forms          (Form, focusedFormInputAttr, formFocus,
                                       formState, handleFormEvent,
                                       invalidFields, invalidFormInputAttr,
                                       renderForm)
import qualified Brick.Types          as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E
import qualified Graphics.Vty         as V

import           Zamonia.Work        (Work, new)
import Zamonia.UI.Internal
import           Zamonia.UI.Series

data Page = PSeries | PSForm
          | PFilm   | PFForm
          | PBook   | PBForm

data AppState e = AppState
    { _mode :: Page
    , _seriesTable :: Page
    , _seriesForm :: SForm e
    {- , _filmTable :: Page
    , _filmForm :: Page
    , _bookTable :: Page
    , _bookForm :: Page -}
    }

makeLenses ''AppState

switchTab :: AppState e -> AppState e
switchTab s = s & mode %~ f
    where f :: Page -> Page
          f PSeries = PFilm
          f PFilm   = PBook
          f PBook   = PSeries

toForm :: AppState e -> AppState e
toForm s = s & mode %~ f
    where f :: Page -> Page
          f PSeries = PSForm
          f PFilm   = PFForm
          f PBook   = PBForm

appEvent :: AppState e -> BrickEvent () e -> T.EventM () (T.Next (AppState e))
appEvent state ev =
    case ev of
        VtyEvent (V.EvResize {})                   -> continue state
        VtyEvent (V.EvKey V.KEsc [])               -> halt state
        VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt state
        VtyEvent (V.EvKey (V.KChar '\t') []) -> continue $ switchTab state
        -- VtyEvent (V.EvKey (V.KChar 'e') []) -> continue $ toForm state -- Edit an entry. Should fail if no entries, otherwise take input from tab
        -- VtyEvent (V.EvKey (V.KChar 'a') []) -> continue $ toForm state -- Add an entry. Should always work and depend of the mode
        _ -> continue state -- No, the state must handle the event
                            -- Need of a case of to specify which one should handle the event

app :: Work w => App (Form w e Fields) e Fields
app =
    App { appDraw = drawForm
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc []) -> halt s
                VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])  -> halt s
                VtyEvent (V.EvKey V.KEnter []) -> halt s
                _ -> handleFormEvent ev s >>= continue
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const attributesMap
        }

form :: Work work => Maybe work -> proxy work -> (work -> Form work e Fields) -> IO work
form initial (_ :: proxy work) mkForm = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
        initialWorkInformations = fromMaybe (new :: work) initial
        f = mkForm initialWorkInformations
    initialVty <- buildVty
    customMain initialVty buildVty Nothing app f <&> formState
