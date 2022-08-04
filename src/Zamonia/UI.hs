{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Zamonia.UI
    where

import           Brick
import qualified Brick.Focus            as F
import           Brick.Forms            (Form, editShowableField, editTextField,
                                         formFocus, formState, handleFormEvent,
                                         newForm, renderForm, (@@=))
import qualified Brick.Forms            as Forms
import           Brick.Main             (continue, continueWithoutRedraw)
import qualified Brick.Types            as T
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import qualified Brick.Widgets.Dialog   as D
import qualified Brick.Widgets.Edit     as E
import           Brick.Widgets.List     (handleListEvent, handleListEventVi)
import qualified Brick.Widgets.List     as L
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor           ((<&>))
import           Data.Maybe             (fromJust, isJust)
import qualified Data.Text              as T
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V
import           Lens.Micro             (traverseOf, (%~), (&), (.~), (?~),
                                         (^.))
import           Lens.Micro.TH
import           Zamonia
import           Zamonia.Work           (Work, new)

makeLenses ''Series

-- | (Index, Status, Name)
type ListItem = (Int, String, String)

type SeriesForm e = Form Series e ResourceName

data WorkForm e = S (SeriesForm e)

data ResourceName = Id -- ^ ID or ISBN
            | Author -- ^ Author or Dirctor
            | EpNumber -- ^ Number of episodes
            | OriginalTitle -- ^ Original Title
            | Possession -- ^ Possession
            | Publisher -- ^ Publisher
            | SeNumber -- ^ Number of seasons
            | Status -- ^ Status
            | Title -- ^ Title
            | Year -- ^ Year of release
            | SeriesTable
            | FilmsTable
            | BooksTable
    deriving (Eq, Ord, Show)

formResources :: [ResourceName]
formResources = [Id, Author, EpNumber, OriginalTitle, Possession, Publisher, SeNumber, Status, Title, Year]

mainFocusList :: [ResourceName]
mainFocusList = [SeriesTable, FilmsTable, BooksTable]

mainFocusRing :: F.FocusRing ResourceName
mainFocusRing = F.focusRing mainFocusList

data AppState e = AppState
  { _focusRing   :: F.FocusRing ResourceName
  , _seriesTable :: L.List ResourceName ListItem
  , _filmTable :: L.List ResourceName ListItem
  , _form        :: Maybe (WorkForm e)
  {- , _help :: Maybe (D.Dialog HelpChoice)
  , _bookTable :: Page -}
  }

makeLenses ''AppState

label :: String -> Widget n -> Widget n
label s w = padTop (Pad 1) (vLimit 1 (hLimit 15 $ str s <+> fill ' ')) <+> B.border w

drawForm :: Work w => Form w e ResourceName -> Widget ResourceName
drawForm f = C.centerLayer form
    where
        form = B.borderWithLabel (str "Edit") $ padTop (Pad 1) $ hLimit 50 $ renderForm f

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.white `on` V.black)
  , (Forms.invalidFormInputAttr, V.brightRed `on` V.black)
  , (Forms.focusedFormInputAttr, V.black `on` V.yellow)
  , (L.listAttr,            V.white `on` V.black)
  , (L.listSelectedAttr,    V.blue `on` V.black)
  , ("listHeader", V.withStyle (V.yellow `on` V.black) V.bold)
  ]

seriesForm :: Series -> SeriesForm e
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

infoWidget :: Widget ResourceName
infoWidget =
      str "a: add   "
  <=> str "c: commit"
  <=> str "e: edit  "
  <=> str "x: remove"
  <=> str "P: purge "
  <+> fill ' '

drawList :: L.List ResourceName ListItem -> Widget ResourceName
drawList l = ui
    where
        label = str "[Series] - Films - Books"
        header = withAttr "listHeader" $ str " Id  Name" <+> strWrap " " <+> str "Status"
        seriesInfo =
          case L.listSelectedElement l of
            Nothing -> fill ' '
            Just (_, (i, s, n)) -> str (show i) <=> str n <=> str s <+> fill ' '
        ui = joinBorders . B.borderWithLabel label $ header <=> L.renderList drawElem True l
                <=> B.hBorder
                <=> vLimit 10 (hLimitPercent 50 seriesInfo
                    <+> B.vBorder
                    <+> hLimitPercent 50 infoWidget)

drawElem :: Bool -> ListItem -> Widget ResourceName
drawElem _ s =
    let (index, status, name) = s
        f x = replicate (3 - length x) ' ' ++ x ++ ": "
     in str (f $ show index) <+> str name <+> strWrap " " <+> str status

initialList :: [ListItem] -> L.List ResourceName ListItem
initialList l = L.list SeriesTable (Vec.fromList l) 1

toSeriesForm :: Bool -> AppState e -> T.EventM ResourceName (T.Next (AppState e))
toSeriesForm empty state =
  if empty then
        let newForm = seriesForm new in
        continue $ state
          & focusRing .~ formFocus newForm
          & form ?~ S newForm
  else
  case L.listSelectedElement (state^.seriesTable) of
    Nothing             -> continueWithoutRedraw state
    Just (_, (n, _, _)) ->
      do
        -- Assume the entry exists
        series <- liftIO $ head <$> connection (`fetchSeries` n)
        let newForm = seriesForm series
        continue $ state
          & focusRing .~ formFocus newForm
          & form ?~ S newForm

handleEvent :: AppState e -> BrickEvent ResourceName e -> T.EventM ResourceName (T.Next (AppState e))
handleEvent state ev =
  let
      focus = F.focusGetCurrent (state^.focusRing)
  in
  case (focus, ev) of
    (_, VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> halt state
    (Just f, VtyEvent (V.EvKey V.KRight []))
      | f `elem` mainFocusList -> continue $ state & focusRing %~ F.focusNext
    (Just f, VtyEvent (V.EvKey V.KLeft []))       -> continue $ state & focusRing %~ F.focusPrev
    (Just SeriesTable, VtyEvent (V.EvKey (V.KChar 'q') []))  -> halt state
    (Just SeriesTable, VtyEvent (V.EvKey V.KEsc [])) -> halt state
    (Just SeriesTable, VtyEvent (V.EvKey (V.KChar 'a') []))  -> toSeriesForm True state
    (Just SeriesTable, VtyEvent (V.EvKey (V.KChar 'e') []))  -> toSeriesForm False state
    (Just SeriesTable, VtyEvent ev) -> continue =<< seriesTable (handleListEventVi handleListEvent ev) state
    (Just FilmsTable, VtyEvent ev) -> continue =<< filmTable (handleListEventVi handleListEvent ev) state
    (Just s, VtyEvent (V.EvKey V.KEsc []))               -> continue (state & form.~ Nothing & focusRing .~ mainFocusRing)
    (Just f, VtyEvent (V.EvKey V.KEnter []))             ->
      if f `elem` formResources then
                                let currentForm = fromJust (state^.form)
                                 in case currentForm of
                                      S f ->
                                        let result = formState f :: Series
                                         in liftIO (connection (`addWork` result))
                                              >> continue (state
                                                            & form .~ Nothing
                                                            & focusRing .~ mainFocusRing)
                                else continue state
    _                                               -> do
                                                        let f (S x) = x
                                                        s' <- handleFormEvent ev (f . fromJust $ state^.form)
                                                        continue (state & form ?~ S s')
      -- VtyEvent (V.EvKey (V.KChar 'a') []) -> continue $ toForm state -- Add an entry. Should always work and depend of the mode

drawUI :: AppState e -> [Widget ResourceName]
drawUI state = case currentFocus of
                 Just SeriesTable -> [drawList (state^.seriesTable)]
                 Just FilmsTable -> [drawList (state^.filmTable)]
                 _ ->
                     case currentForm of
                       Just (S sf) -> [drawForm sf, drawList (state^.seriesTable)]
                       _ -> error "Impossible"
    where
        currentFocus = F.focusGetCurrent (state^.focusRing)
        currentForm = state^.form

appCursor :: AppState e -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
appCursor state =
  case state^.form of
    Just (S s) -> F.focusRingCursor formFocus s
    _      -> F.focusRingCursor (^.focusRing) state

initState :: IO (AppState e)
initState = filmTable f =<< (seriesTable s $ AppState
  {
    _focusRing = mainFocusRing
  , _seriesTable = undefined -- Not very clean
  , _filmTable = undefined
  , _form = Nothing
  })
    where
      s _ = connection $ \c -> listSeries Names c <&> initialList
      f _ = connection $ \c -> listFilms Names c <&> initialList

app :: App (AppState e) e ResourceName
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appChooseCursor = appCursor
        , appStartEvent = pure
        , appAttrMap = const attributesMap
        }

main :: IO ()
main = void $ initState >>= defaultMain app
