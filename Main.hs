{-# LANGUAGE OverloadedStrings #-}

module Main where

import Slide

import System.Environment
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Brick.Types
import Brick.Widgets.Core
import Brick.Util (fg, on)
import qualified Brick.Main as M
import qualified Data.Vector as V
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Text.Wrap
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.List as L

type AppState = L.List () Slide

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "USAGE: maxpresentation slides_directory"
    ["--version"] -> putStrLn "version: 5"
    (dir:_) -> do
      slides <- readSlides dir
      void $ M.defaultMain app $ L.list () (V.fromList slides) 1

app :: M.App AppState e ()
app = M.App {
    M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrs
  }

attrs :: A.AttrMap
attrs = A.attrMap V.defAttr [
    (L.listSelectedAttr, V.black `on` V.green)
  , (titleAttr, V.white `on` V.blue)
  , ("red", fg V.blue)
  , ("ita", fg V.green)
  , ("key", fg V.brightMagenta)
  ]

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    V.EvKey (V.KChar 'q') [] -> M.halt l
    ev -> (L.handleListEventVi L.handleListEvent) ev l >>= M.continue
appEvent l _ = M.continue l

drawUI :: L.List () Slide -> [Widget ()]
drawUI l = [ui]
  where
    Just (_, slide) = L.listSelectedElement l
    slides = B.borderWithLabel (str " slides ")
           $ hLimit 16 $ vLimit 17
           $ L.renderList listDrawElement True l
    speaker = C.vBox [
                B.hBorderWithLabel (str " Speaker Notes ")
              , vLimit 4 $ strWrap $ notes slide
              ]
    slideSlide = vLimit 60 $ mkSlideContent (slideContent slide)
    slideText = C.withAttr titleAttr
              $ C.overrideAttr B.borderAttr titleAttr
              $ B.hBorderWithLabel $ str $ " " ++ (slideTitle slide) ++ " "
    oneDiv = hLimit 1 $ hBox [str " "]
    ui = C.vBox [C.hBox [slides, oneDiv, C.vBox [slideText, slideSlide], oneDiv], speaker]

listDrawElement :: Bool -> Slide -> Widget ()
listDrawElement sel a = C.padRight Max $ str $ title a

mkSlideContent :: [(String, Attr)] -> Widget ()
mkSlideContent xs = C.vBox $ widgeto $ lineify xs
  where
    extraSpacingLineAtTop = [[]] -- just [] for no extra line
    lineify :: [(String, Attr)] -> [[(String, Attr)]]
    lineify xs = reverse $ foldl f extraSpacingLineAtTop (xs ++ [("\n", NoneAttr)])
      where
        f (y:ys) ("\n", _) = ([]:(reverse y):ys)
        f (y:ys) h = (h:y):ys
        f [] ("\n", _) = [[]]
        f [] h = [[h]]
    
    widgeto :: [[(String, Attr)]] -> [Widget ()]
    widgeto xs = map (C.hBox . lineo) xs
    
    lineo [] = [str " "]
    lineo xs = map light xs
    
    light (a, s) = C.withAttr (attrNamed s) (str a)

titleAttr :: A.AttrName
titleAttr = "window" <> "border"

attrNamed :: Attr -> A.AttrName
attrNamed RedAttr = "red"
attrNamed ItaAttr = "ita"
attrNamed KeyAttr = "key"
attrNamed NoneAttr = "none"
