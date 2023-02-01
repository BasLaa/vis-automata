{-# LANGUAGE OverloadedStrings #-}

module Main where

import FSMGraph

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.HTTP.Types as HTTP
import qualified Control.Concurrent.STM as STM

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T

import qualified Data.Graph.Inductive as GI
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete

import Data.Map
import System.Environment (getEnv)

data MyState = MyState
    { msFSM :: FSM
    , msStText :: String
    , msStartText :: String
    , msAccText :: String
    , msTransText :: String
    }

makeDummyState :: MyState
makeDummyState = 
  let fsm = FSM (mkStates ["1", "2", "3"]) 1 [(1, 2, "a"), (2, 2, "a,b"), (1, 3, "b"), (3, 3, "a,b")] [1, 2]
  in MyState fsm "1 2 3" "1" "1 2" "1 2 a; 2 2 a,b; 1 3 b; 3 3 a,b"

main :: IO ()
main = do
  mystateVar <- STM.newTVarIO makeDummyState
  port <- read <$> getEnv "PORT"
  scotty port (myApp mystateVar)

myApp :: STM.TVar MyState -> ScottyM ()
myApp statevar = do
  get "/" $ do
    currstate <- liftIO $ STM.readTVarIO statevar
    let dotGraph = graphToDot (gParams $ accepts $ msFSM currstate) (graphFSM $ msFSM currstate)
    _ <- liftIO $ runGraphvizCommand Dot dotGraph Png "finite-automaton.png"
    html $ renderHtml $ H.docTypeHtml $
      H.html $ do
        H.head $ H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "style.css"
        H.body $ do
          H.h1 "Finite Automata Visualizer"
          H.div H.! A.class_ "container" $ do

            H.div H.! A.class_ "form-container" $ do
              H.form H.! A.method "post" H.! A.action "/" $ do
                H.div H.! A.class_ "form-group" $ do
                  H.label H.! A.for "states" H.! A.name "states" $ "States: "
                  H.input H.! A.type_ "text" H.! A.value (H.toValue $ msStText currstate) H.! A.id "states" H.! A.name "states"
                H.div H.! A.class_ "form-group" $ do
                  H.label H.! A.for "start" H.! A.name "start" $ "Start state: "
                  H.input H.! A.type_ "text" H.! A.value (H.toValue $ msStartText currstate) H.! A.id "start" H.! A.name "start"
                H.div H.! A.class_ "form-group" $ do
                  H.label H.! A.for "accepts" H.! A.name "accepts" $ "Accept states: "
                  H.input H.! A.type_ "text" H.! A.value (H.toValue $ msAccText currstate) H.! A.id "accepts" H.! A.name "accepts"
                H.div H.! A.class_ "form-group" $ do
                  H.label H.! A.for "transitions" H.! A.name "transitions" $ "Transitions : "
                  H.input H.! A.type_ "text" H.! A.value (H.toValue $ msTransText currstate) H.! A.id "transitions" H.! A.name "transitions"
                H.div H.! A.class_ "form-group" H.! A.id "but" $ do
                  H.input H.! A.type_ "submit" H.! A.value "Create Automaton"
              
              H.div H.! A.id "notes" $ do
                H.h2 "Notes: "
                H.ul $ do
                  H.li "States and Accept States should be separated by spaces"
                  H.li "There may only be a single Start State"
                  H.li "Transitions should be separated by semicolons and of the form \"state state symbol\""

            H.div H.! A.class_ "image-container" $ do
              H.img H.! A.src "/image.png"
              H.form H.! A.method "post" H.! A.id "download-group" H.! A.action "/download" $ do
                H.div H.! A.class_ "form-group" $ do
                  H.fieldset $ do
                    H.legend "Select a file type: "
                    H.div $ do
                      H.label $ do
                        H.input H.! A.type_ "radio" H.! A.name "filetype" H.! A.value "png" H.! A.checked "checked"
                        "PNG"
                    H.div $ do
                      H.label $ do
                        H.input H.! A.type_ "radio" H.! A.name "filetype" H.! A.value "jpeg"
                        "JPEG"
                    H.div $ do
                      H.label $ do
                        H.input H.! A.type_ "radio" H.! A.name "filetype" H.! A.value "svg"
                        "SVG"
                    H.div $ do
                      H.label $ do
                        H.input H.! A.type_ "radio" H.! A.name "filetype" H.! A.value "dot"
                        "Dot"                
                H.div H.! A.class_ "form-group" H.! A.id "but" $ do
                  H.input H.! A.type_ "submit" H.! A.value "Download"


  get "/style.css" $ file "app/style.css"

  post "/" $ do
    states <- param "states" `rescue` const (return "")
    start <- param "start" `rescue` const (return "")
    accept <- param "accepts" `rescue` const (return "")
    transitions <- param "transitions" `rescue` const (return "")
    let idstates = mkStates $ words states
    let idstart = idstates ! start
    let parsedTrans = mkTransitions (wordsWhen (== ';')  transitions) idstates
    case parsedTrans of
      Nothing -> do
        status HTTP.notFound404
        text "Incorrect transitions"
      Just p -> do
        let idaccepts = fmap (idstates !) (words  accept)
        let fsm = FSM idstates idstart p idaccepts
        liftIO $ STM.atomically $ STM.writeTVar statevar (MyState fsm states start accept transitions)
        redirect "/"
  
  post "/download" $ do
    filetype <- param ("filetype" :: T.Text) `rescue` const (return "") :: ActionM T.Text
    currstate <- liftIO $ STM.readTVarIO statevar
    case filetype of
      "png" -> do
        setHeader "Content-Disposition" "attachment; filename=finite-automaton.png"
        file "finite-automaton.png"
      "svg" -> do
        let dotGraph = graphToDot (gParams $ accepts $ msFSM currstate) (graphFSM $ msFSM currstate)
        _ <- liftIO $ runGraphvizCommand Dot dotGraph Svg "finite-automaton.svg"
        setHeader "Content-Disposition" "attachment; filename=finite-automaton.svg"
        file "finite-automaton.png"
      "jpeg" -> do
        let dotGraph = graphToDot (gParams $ accepts $ msFSM currstate) (graphFSM $ msFSM currstate)
        _ <- liftIO $ runGraphvizCommand Dot dotGraph Jpeg "finite-automaton.jpeg"
        setHeader "Content-Disposition" "attachment; filename=finite-automaton.jpeg"
        file "finite-automaton.jpeg"
      "dot" -> do
        setHeader "Content-Disposition" "attachment; filename=finite-automaton.dot"
        file "finite-automaton.dot"
      _ -> error "help"
    redirect "/"
  
  get "/image.png" $ file "finite-automaton.png"
