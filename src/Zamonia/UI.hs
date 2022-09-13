{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Zamonia.UI
  (main)
    where

import           Brick
import qualified Brick.Focus            as F
import           Brick.Forms            (Form, editShowableField, editTextField,
                                         formFocus, formState, handleFormEvent,
                                         newForm, renderForm, (@@=))
import qualified Brick.Forms            as Forms
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import qualified Brick.Widgets.Dialog   as D
import qualified Brick.Widgets.Edit     as E
import qualified Brick.Widgets.List     as L
import           Control.Arrow          ((&&&))
import           Control.Monad          (join, liftM2, void, (<=<))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.Vector            as Vec
import           Database.SQLite.Simple
import qualified Graphics.Vty           as V
import           Lens.Micro             (ASetter, Getting, _3, (%~), (&), (.~),
                                         (?~), (^.))
import           Lens.Micro.TH
import           System.Process.Typed   (ProcessConfig, proc, runProcess)
import           Zamonia
import           Zamonia.Work           (Work, listRepresentation, new)

makeLenses ''Series
makeLenses ''Film
makeLenses ''Book

-- | (Index, Status, Name)
type ListItem = (Int, T.Text, T.Text)
type WorkList = L.List ResourceName ListItem

type SeriesForm e = Form Series e ResourceName
type FilmForm e = Form Film e ResourceName
type BookForm e = Form Book e ResourceName

data WorkForm e = S (SeriesForm e) | F (FilmForm e) | B (BookForm e)

data ResourceName = Id -- ^ ID or ISBN
            | Author -- ^ Author or Dirctor
            | EpNumber -- ^ Number of episodes
            | OriginalTitle -- ^ Original Title
            | Possession -- ^ Possession
            | Publisher -- ^ Publisher
            | ISBN -- ^ ISBN
            | SeNumber -- ^ Number of seasons
            | Status -- ^ Status
            | Title -- ^ Title
            | Year -- ^ Year of release
            | SeriesTable
            | FilmsTable
            | BooksTable
            | RemoveDialog (ListItem, ResourceName)
            | InfoDialog (String, ResourceName)
            | Help
    deriving (Eq, Ord, Show)

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

formResources :: [ResourceName]
formResources = [Id, Author, EpNumber, OriginalTitle, Possession, Publisher, SeNumber, Status, Title, Year]

mainFocusList :: [ResourceName]
mainFocusList = [SeriesTable, FilmsTable, BooksTable]

mainFocusRing :: F.FocusRing ResourceName
mainFocusRing = F.focusRing mainFocusList

data AppState e = AppState
  { _previousFocusRing :: Maybe (F.FocusRing ResourceName)
  , _focusRing         :: F.FocusRing ResourceName
  , _seriesTable       :: WorkList
  , _filmTable         :: WorkList
  , _bookTable         :: WorkList
  , _form              :: Maybe (WorkForm e)
  , _removingDialog    :: Maybe (D.Dialog (Maybe Int))
  , _infoDialog        :: Maybe (D.Dialog ())
  , _conn              :: Connection
  -- , _help :: Maybe (D.Dialog HelpChoice) -- TODO
  }

makeLenses ''AppState

label :: String -> Widget n -> Widget n
label s w = padTop (Pad 1) (vLimit 1 (hLimit 15 $ str s <+> fill ' ')) <+> B.border w

drawForm :: Work w => Form w e ResourceName -> Widget ResourceName
drawForm f = C.centerLayer f'
    where
        f' = B.borderWithLabel (str "Edit") $ padTop (Pad 1) $ hLimit 50 $ renderForm f

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.white `on` V.black)
  , (Forms.invalidFormInputAttr, V.brightRed `on` V.black)
  , (Forms.focusedFormInputAttr, V.black `on` V.yellow)
  , (L.listAttr, V.white `on` V.black)
  , (L.listSelectedAttr, V.blue `on` V.black)
  , (D.dialogAttr, V.white `on` V.black)
  , (D.buttonAttr, V.black `on` V.white)
  , (D.buttonSelectedAttr, V.black `on` V.yellow)
  , ("listHeader", V.withStyle (V.yellow `on` V.black) V.bold)
  , ("windowTop", V.withStyle (V.green `on` V.black) V.bold)
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
                editTextField sdirector Author (Just 1)
            , label "Year" @@=
                editTextField syear Year (Just 1)
            , label "Episode Number" @@=
                editTextField epNumber EpNumber (Just 1)
            , label "Season Number" @@=
                editTextField seNumber SeNumber (Just 1)
            , label "Possession" @@=
                editTextField spossession Possession (Just 1)
            , label "Status" @@=
                editTextField swatched Status (Just 1)
            ]

filmForm :: Film -> FilmForm e
filmForm =
  newForm [ label "Id" @@=
                editShowableField fid Id
            , label "Title" @@=
                editTextField ftitle Title (Just 1)
            , label "Original Title" @@=
                editTextField foriginalTitle OriginalTitle (Just 1)
            , label "Director" @@=
                editTextField fdirector Author (Just 1)
            , label "Year" @@=
                editTextField fyear Year (Just 1)
            , label "Possession" @@=
                editTextField fpossession Possession (Just 1)
            , label "Status" @@=
                editTextField fwatched Status (Just 1)
            ]

bookForm :: Book -> BookForm e
bookForm =
    newForm [ label "Id" @@=
                editShowableField bid Id
            , label "ISBN" @@=
                editTextField bisbn ISBN (Just 1)
            , label "Title" @@=
                editTextField btitle Title (Just 1)
            , label "Original Title" @@=
                editTextField boriginalTitle OriginalTitle (Just 1)
            , label "Author" @@=
                editTextField bauthor Author (Just 1)
            , label "Publisher" @@=
                editTextField bpublisher Publisher (Just 1)
            , label "Year" @@=
                editTextField byear Year (Just 1)
            , label "Possession" @@=
                editTextField bpossession Possession (Just 1)
            , label "Status" @@=
                editTextField bread Status (Just 1)
            ]

infoWidget :: Widget ResourceName
infoWidget =
      str "a: add   "
  <=> str "e: edit  "
  <=> str "x: remove"
  <=> str "s: sort  "
  <=> str "n: note  "
  <+> fill ' '

drawList :: WorkList -> Widget ResourceName
drawList l = ui
    where
        top = withAttr "windowTop" $
          case L.listName l of
            SeriesTable -> str " [Series] - Films - Books "
            FilmsTable  -> str " Series - [Films] - Books "
            BooksTable  -> str " Series - Films - [Books] "
            _           -> error "drawList: focus is not right"
        header = withAttr "listHeader" $ str " Id  Name" <+> strWrap " " <+> str "Status"
        seriesInfo =
          case L.listSelectedElement l of
            Nothing             -> fill ' '
            Just (_, (i, s, n)) -> str (show i) <=> txt n <=> txt s <+> fill ' '
        ui = joinBorders . B.borderWithLabel top $ header <=> L.renderList drawElem True l
                <=> B.hBorder
                <=> vLimit 10 (hLimitPercent 50 seriesInfo
                    <+> B.vBorder
                    <+> hLimitPercent 50 infoWidget)

drawElem :: Bool -> ListItem -> Widget ResourceName
drawElem _ s =
    let (index, status, name) = s
        f x = replicate (3 - length x) ' ' ++ x ++ ": "
     in str (f $ show index) <+> txt name <+> strWrap " " <+> txt status

initialList :: ResourceName -> [ListItem] -> WorkList
initialList n l = L.list n (Vec.fromList l) 1

indexInsertList :: ListItem -> WorkList -> WorkList -- TODO: modify when sorting will be toggable
indexInsertList e@(_, _, n) l = go 0 end
  where
    v :: Vec.Vector ListItem
    v = L.listElements l
    end :: Int
    end = Vec.length v
    go :: Int -> Int -> WorkList
    go a b
      | a >= b = L.listInsert a e l
      | otherwise =
        let c = (a + b) `div` 2
            x = (v Vec.! c) ^. _3
         in if x > n then go a (c-1)
                     else if x < n then go (c+1) b
                     else go c c

showInformation :: forall e. ResourceName -> AppState e -> EventM ResourceName (Next (AppState e))
showInformation focus state = maybe (continueWithoutRedraw state) h entry
  where
    entry :: Maybe (ListItem, EventM ResourceName String)
    entry =
      case focus of
        SeriesTable ->
          f (Proxy @Series) IdS <$> L.listSelectedElement (state^.seriesTable)
        FilmsTable  ->
          f (Proxy @Film) IdF <$> L.listSelectedElement (state^.filmTable)
        BooksTable  ->
          f (Proxy @Book) IdB <$> L.listSelectedElement (state^.bookTable)
        _           -> error "showInformation: focus is not right"
    f :: (FromRow w, Show w, Work w) => proxy w -> (Int -> Id) -> (Int, ListItem) -> (ListItem, EventM ResourceName String)
    f p cons = (id &&& g p . cons . fst') . snd
    g :: (FromRow w, Show w, Work w) => proxy w -> Id -> EventM ResourceName String
    g (_ :: proxy w) = liftIO . (show . (head @w) <$>) . fetchWork (state^.conn)
    h :: (ListItem, EventM ResourceName String) -> EventM ResourceName (Next (AppState e))
    h ((i, _, t), s) =
      let !prevFocus = state^.focusRing
       in (\s' -> continue (state & previousFocusRing ?~ prevFocus
                                  & infoDialog ?~ informationDialog i t
                                  & focusRing .~ (F.focusRing . (:[]) . InfoDialog $ (s',focus))
                     )) =<< s

informationDialog :: Int -> T.Text -> D.Dialog ()
informationDialog i t = D.dialog (Just (show i ++ ": " ++ T.unpack t)) (Just (0, [("Ok", ())])) 80

renderInfoDialog :: String -> Maybe (D.Dialog ()) -> Widget ResourceName
renderInfoDialog w (Just d) =
  D.renderDialog d
    $ padAll 1
    $ vBox
        [ C.hCenter (str w) ]
renderInfoDialog _ _ = str ""

removeDialog :: Int -> D.Dialog (Maybe Int)
removeDialog n = D.dialog (Just "Remove entry ?") (Just (0, choices)) 75
  where
    choices :: [(String, Maybe Int)]
    choices = [("Yes", Just n), ("No", Nothing)]

renderRemoveDialog :: ListItem -> Maybe (D.Dialog (Maybe Int)) -> Widget ResourceName
renderRemoveDialog (_, _, t) (Just d) =
      D.renderDialog d
        $ padAll 1
        $ vBox
            [ C.hCenter (str "Do you want to remove the entry ?")
            , vLimit 1 (fill ' ')
            , C.hCenter (txt t)
            ]
renderRemoveDialog _ _ = str ""

removeEntry :: ResourceName -> AppState e -> EventM ResourceName (Next (AppState e))
removeEntry focus state =
  let entry = L.listSelectedElement $ state ^. (case focus of
                SeriesTable -> seriesTable
                FilmsTable  -> filmTable
                BooksTable  -> bookTable
                _           -> error "removeEntry: focus is not right")
   in maybe (continueWithoutRedraw state) f entry
  where
    f (n, x) =
      let !prevRing = state^.focusRing in
      continue (state & removingDialog ?~ removeDialog n
                      & previousFocusRing ?~ prevRing
                      & focusRing .~ F.focusRing [RemoveDialog (x, focus)]
               )

editNote :: forall e. ResourceName -> AppState e -> EventM ResourceName (Next (AppState e))
editNote focus state =
  case num of
    Nothing -> continueWithoutRedraw state
    Just n -> liftIO (command n) >>= runProcess >> continue state
  where
    f :: Getting WorkList (AppState e) WorkList -> Maybe Int
    f = ((fst' . snd) <$>) . L.listSelectedElement . (state^.)
    location :: IO FilePath
    num :: Maybe Int
    (!location, !num) = case focus of
                        SeriesTable -> (seriesLocation, f seriesTable)
                        FilmsTable -> (filmsLocation, f filmTable)
                        BooksTable -> (booksLocation, f bookTable)
                        _ -> error "editNot: focus is not right"
    command :: Int ->  IO (ProcessConfig () () ())
    command n = liftM2 (\e l -> proc e [l ++ (show n ++ ".md")]) editor location -- FIXME: not nice

appendForm :: ResourceName -> AppState e -> EventM ResourceName (Next (AppState e))
appendForm focus state =
  let (focus', form') =
                  case focus of
                    SeriesTable ->
                      let newSeriesForm = seriesForm (new & sid .~ (inc . Vec.foldr f Nothing . L.listElements $ state^.seriesTable))
                       in (formFocus newSeriesForm, S newSeriesForm)
                    FilmsTable  ->
                      let newFilmForm = filmForm (new & fid .~ (inc . Vec.foldr f Nothing . L.listElements $ state^.filmTable))
                       in (formFocus newFilmForm, F newFilmForm)
                    BooksTable  ->
                      let newBookForm = bookForm (new & bid .~ (inc . Vec.foldr f Nothing . L.listElements $ state^.bookTable))
                       in (formFocus newBookForm, B newBookForm)
                    _           -> error "appendForm: focus is not right"
      !prevRing = state^.focusRing
   in continue $ state
        & previousFocusRing ?~ prevRing
        & focusRing .~ focus'
        & form ?~ form'
    where
      f :: ListItem -> Maybe Int -> Maybe Int
      f (n, _, _) Nothing  = Just n
      f (n, _, _) (Just x) = Just $ max x n
      inc :: Maybe Int -> Int
      inc = maybe 1 (+1)

editForm :: ResourceName -> AppState e -> EventM ResourceName (Next (AppState e)) -- TODO: FIXME
editForm focus state =
  let getter =
        case focus of
                 SeriesTable -> seriesTable
                 FilmsTable  -> filmTable
                 BooksTable  -> bookTable
                 _           -> error "editForm: focus is not right"
   in
   case L.listSelectedElement (state^.getter) of
    Nothing             -> continueWithoutRedraw state
    Just (_, (n, _, _)) ->
      -- Assumes the entry exists
      case focus of -- TODO shorter version ?
        SeriesTable ->
          liftIO (seriesForm . head <$> fetchWork (state^.conn) (IdS n))
          >>= \item -> continue $ state
            & previousFocusRing ?~ (state^.focusRing)
            & focusRing .~ formFocus item
            & form ?~ S item
        FilmsTable ->
          liftIO (filmForm . head <$> fetchWork (state^.conn) (IdF n))
          >>= \item -> continue $ state
            & previousFocusRing ?~ (state^.focusRing)
            & focusRing .~ formFocus item
            & form ?~ F item
        BooksTable ->
          liftIO (bookForm . head <$> fetchWork (state^.conn) (IdB n))
          >>= \item -> continue $ state
            & previousFocusRing ?~ (state^.focusRing)
            & focusRing .~ formFocus item
            & form ?~ B item
        _ -> error "editForm: focus is not right"


-- TODO: Test requery of the whole list instead of inserting
closeForm :: forall e. AppState e -> EventM ResourceName (Next (AppState e))
closeForm state =
  case currentForm of
    S sForm ->
      let result = formState sForm :: Series
       in addAndReturn result seriesTable
    F fForm ->
      let result = formState fForm :: Film
       in addAndReturn result filmTable
    B bForm ->
      let result = formState bForm :: Book
       in addAndReturn result bookTable
    where
      c :: Connection
      c = state^.conn
      currentForm :: WorkForm e
      currentForm = fromJust $ state^.form
      addAndReturn :: Work w => w -> ASetter (AppState e) (AppState e) WorkList WorkList -> EventM ResourceName (Next (AppState e))
      addAndReturn res getter = liftIO (addWork c res) >>
        let !prevRing = fromJust $ state^.previousFocusRing
         in
        continue (state
          & form .~ Nothing
          & focusRing .~ prevRing
          & getter %~ indexInsertList (listRepresentation res))

handleEvent :: forall e. AppState e -> BrickEvent ResourceName e -> EventM ResourceName (Next (AppState e))
handleEvent state event =
  let
      focus = F.focusGetCurrent (state^.focusRing)
  in
  if focus `elem` map Just mainFocusList then
    case event of
      VtyEvent (V.EvKey V.KRight [])      -> continue $ state & focusRing %~ F.focusNext
      VtyEvent (V.EvKey V.KLeft [])       -> continue $ state & focusRing %~ F.focusPrev
      VtyEvent (V.EvKey (V.KChar 'q') []) -> halt state
      VtyEvent (V.EvKey V.KEsc []) -> halt state
      VtyEvent (V.EvKey (V.KChar 'a') []) -> appendForm (fromJust focus) state
      VtyEvent (V.EvKey (V.KChar 'e') []) -> editForm (fromJust focus) state
      VtyEvent (V.EvKey (V.KChar 'x') []) -> removeEntry (fromJust focus) state
      VtyEvent (V.EvKey (V.KChar 'n') []) -> editNote (fromJust focus) state
      VtyEvent (V.EvKey V.KEnter [])      -> showInformation (fromJust focus) state
      VtyEvent ev ->
        let handledEvent = L.handleListEventVi L.handleListEvent ev
            table = case focus of
                      Just SeriesTable -> seriesTable
                      Just FilmsTable  -> filmTable
                      Just BooksTable  -> bookTable
                      _                -> error "handleEvent: focus is not right"
         in continue =<< table handledEvent state
      _ -> undefined -- TODO
  else
    case (focus, event) of
      (_, VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) -> halt state
      (Just (RemoveDialog ((i, _, _), orig)), VtyEvent (V.EvKey V.KEnter [])) ->
        maybe (continue state') f dialogResult
        where
          !prevFocus = fromJust (state^.previousFocusRing)
          !dialogResult = join $ state^.removingDialog >>= D.dialogSelection
          state' = state & focusRing .~ prevFocus
                         & previousFocusRing .~ Nothing
                         & removingDialog .~ Nothing
          f :: Int -> EventM ResourceName (Next (AppState e))
          f n =
            case orig of -- TODO: FIXME
              SeriesTable ->
                liftIO (delWork (state^.conn) (IdS i))
                >> continue (state' & seriesTable %~ L.listRemove n)
              FilmsTable  ->
                liftIO (delWork (state^.conn) (IdF i))
                >> continue (state' & filmTable %~ L.listRemove n)
              BooksTable  ->
                liftIO (delWork (state^.conn) (IdB i))
                >> continue (state' & bookTable %~ L.listRemove n)
              _ -> error "handleEvent: (in dialog) focus is not right"
      (Just (InfoDialog _), VtyEvent (V.EvKey V.KEnter [])) ->
        let !prevFocus = fromJust (state^.previousFocusRing)
         in continue (state & focusRing .~ prevFocus
                            & previousFocusRing .~ Nothing
                            & infoDialog .~ Nothing)
      (Just (RemoveDialog _), VtyEvent ev) ->
        case state^.removingDialog of
          Nothing -> continue state
          Just dialog ->
            do
              newDialog <- D.handleDialogEvent ev dialog
              continue (state & removingDialog ?~ newDialog)
      (Just (InfoDialog _), VtyEvent ev) ->
        case state^.infoDialog of
          Nothing -> continue state
          Just dialog ->
            do newDialog <- D.handleDialogEvent ev dialog
               continue (state & infoDialog ?~ newDialog)
      (Just _, VtyEvent (V.EvKey V.KEsc [])) ->
        let !prevRing = fromJust (state^.previousFocusRing)
         in continue (state
                    & form.~ Nothing
                    & previousFocusRing .~ Nothing
                    & focusRing .~ prevRing)
      (Just _, VtyEvent (V.EvKey V.KEnter [])) -> closeForm state
      _ -> do
        w' <- f . fromJust $ state^.form
        continue (state & form ?~ w')
          where
            g :: Form s e ResourceName -> EventM ResourceName (Form s e ResourceName)
            g = handleFormEvent event
            f :: WorkForm e -> EventM ResourceName (WorkForm e)
            f (S x) = S <$> g x
            f (F x) = F <$> g x
            f (B x) = B <$> g x

drawUI :: AppState e -> [Widget ResourceName]
drawUI state =
  case currentForm of
    Nothing     -> background currentFocus
    Just (S sf) -> drawForm sf:background SeriesTable
    Just (F ff) -> drawForm ff:background FilmsTable
    Just (B bf) -> drawForm bf:background BooksTable
    where
      currentFocus = fromJust $ F.focusGetCurrent (state^.focusRing)
      currentForm = state^.form
      background :: ResourceName -> [Widget ResourceName]
      background =
        \case SeriesTable -> [drawList (state^.seriesTable)]
              FilmsTable -> [drawList (state^.filmTable)]
              BooksTable -> [drawList (state^.bookTable)]
              (RemoveDialog (x, table)) ->
                renderRemoveDialog x (state^.removingDialog):background table
              (InfoDialog (w, table)) ->
                renderInfoDialog w (state^.infoDialog):background table
              _           -> error "drawUI: focus is not right"

appCursor :: AppState e -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
appCursor state =
  let g = F.focusRingCursor formFocus in
  case state^.form of
    Just (S s) -> g s
    Just (F f) -> g f
    Just (B b) -> g b
    _          -> F.focusRingCursor (^.focusRing) state

initState :: Connection -> IO (AppState e)
initState c = do
  sList <- initialList SeriesTable <$> listSeries Names c
  fList <- initialList FilmsTable <$> listFilms Names c
  bList <- initialList BooksTable <$> listBooks Names c
  return $
    AppState { _previousFocusRing = Nothing
             , _focusRing = mainFocusRing
             , _removingDialog = Nothing
             , _infoDialog = Nothing
             , _seriesTable = sList
             , _filmTable = fList
             , _bookTable = bList
             , _form = Nothing
             , _conn = c
             }

app :: App (AppState e) e ResourceName
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appChooseCursor = appCursor
        , appStartEvent = pure
        , appAttrMap = const attributesMap
        }

main :: IO ()
main = void $ connection (defaultMain app <=< initState)
