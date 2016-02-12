{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GADTs #-}

module ReactiveHtml
       (runReactiveHtml, li_, ul_, div_, ReactiveHtml, Html, button_,
        joinHtml, html, render, onInput, onClick, text_, input_)
       where

import Control.Monad
import Data.Foldable (traverse_,for_)
import Data.Maybe
import Data.Monoid
import qualified Data.Patch as Patch
import Data.String (IsString(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.CharacterData (setData)
import GHCJS.DOM.Document (createElement, createTextNode, getBody)
import GHCJS.DOM.Element (castToElement, querySelectorAll)
import GHCJS.DOM.Event (getTarget)
import GHCJS.DOM.EventTarget
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Node
       (Node, appendChild, castToNode, insertBefore, replaceChild,
        removeChild)
import GHCJS.DOM.NodeList (item)
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.UIEvent
import GHCJS.Marshal (ToJSVal, FromJSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import qualified GHCJS.Prim as Prim
import Reactive.Banana
import Reactive.Banana.Frameworks

newtype EqNode = EqNode Node
  deriving (DOM.IsNode, DOM.IsEventTarget, DOM.IsGObject, ToJSVal, FromJSVal)

foreign import javascript unsafe "$1 === $2"
  js_eqRef :: Prim.JSVal -> Prim.JSVal -> Bool

instance Eq EqNode where
  EqNode l == EqNode r =
    case (pToJSVal l,pToJSVal r) of
      (a,b) -> js_eqRef a b

newtype ReactiveHtml a =
  ReactiveHtml (MomentIO (a,Behavior (Vector EqNode)))

instance Monoid a => Monoid (ReactiveHtml a) where
  mempty = ReactiveHtml (return (mempty,pure mempty))
  mappend (ReactiveHtml a) (ReactiveHtml b) =
    ReactiveHtml (liftA2 (\(u,x) (v,y) -> (u <> v,liftA2 mappend x y)) a b)

instance Functor ReactiveHtml where
  fmap = liftM

instance Applicative ReactiveHtml where
  pure = return
  (<*>) = ap

instance Monad ReactiveHtml where
  return a = ReactiveHtml (return (a,pure mempty))
  ReactiveHtml m >>= f =
    ReactiveHtml
      (do (a,nodes) <- m
          (b,nodes') <-
            case f a of
              ReactiveHtml m' -> m'
          return (b,liftA2 (<>) nodes nodes'))

instance a ~ () => IsString (ReactiveHtml a) where
  fromString str =
    ReactiveHtml $
    do Just document <- liftIO currentDocument
       Just tNode <- liftIO (createTextNode document str)
       return ((),pure (pure (EqNode (castToNode tNode))))

text_ :: Behavior String -> ReactiveHtml ()
text_ contents =
  ReactiveHtml $
  do initialText <- valueBLater contents
     Just textNode <-
       liftIO (do Just document <- currentDocument
                  createTextNode document
                                 (mempty `asTypeOf` initialText))
     liftIOLater (setData textNode (Just initialText))
     contentsChanged <- changes contents
     reactimate' (fmap (fmap (setData textNode . Just)) contentsChanged)
     return ((),pure (pure (EqNode (castToNode textNode))))

div_, ul_, li_, button_
  :: ReactiveHtml a -> ReactiveHtml ()
div_ = makeElement "div"
ul_ = makeElement "ul"
li_ = makeElement "li"
button_ = makeElement "button"

makeElement
  :: String -> ReactiveHtml a -> ReactiveHtml ()
makeElement el (ReactiveHtml mkChildren) =
  ReactiveHtml $
  do Just divElement <-
       liftIO (do Just document <- currentDocument
                  createElement document
                                (Just el :: Maybe String))
     (_,children) <- mkChildren
     initialChildren <- valueB children
     liftIO (traverse_ (void . appendChild divElement . Just . id) initialChildren)
     childrenChanged <- changes children
     reactimate'
       ((\old getNew ->
           do new <- getNew
              let edits = Patch.toList (Patch.diff old new)
              return (for_ edits
                           (\edit ->
                              case edit of
                                Patch.Insert n node ->
                                  insertBefore divElement
                                               (Just (id node))
                                               (fmap id (old V.!? n))
                                Patch.Replace _ oldNode newNode ->
                                  replaceChild divElement
                                               (Just (id newNode))
                                               (Just (id oldNode))
                                Patch.Delete _ node ->
                                  removeChild divElement
                                              (Just (id node))))) <$>
        children <@> childrenChanged)
     return ((),pure (pure (EqNode (castToNode divElement))))

input_ :: Behavior String -> ReactiveHtml ()
input_ value =
  ReactiveHtml $
  do Just document <- liftIO currentDocument
     Just el <-
       liftIO (createElement document
                             (Just "input" :: Maybe String))
     (input,fixInput) <- newEvent
     eventListener <-
       liftIO (eventListenerNew (const (fixInput ()) :: UIEvent -> IO ()))
     liftIOLater
       (addEventListener el
                         ("input" :: String)
                         (Just eventListener)
                         False)
     initialValue <- valueB value
     liftIO (setValue (castToHTMLInputElement el)
                      (Just initialValue))
     valueChanged <- changes value
     reactimate' (fmap (fmap (setValue (castToHTMLInputElement el) . Just)) valueChanged)
     reactimate
       (fmap (setValue (castToHTMLInputElement el) . Just)
             (value <@ input))
     return ((),pure (pure (EqNode (castToNode el))))

newtype Html =
  Html (Behavior (Vector EqNode))

instance Monoid Html where
  mempty = Html (pure mempty)
  Html l `mappend` Html r = Html (liftA2 mappend l r) 

render :: ReactiveHtml a -> MomentIO Html
render (ReactiveHtml mk) = fmap (Html . snd) mk

onInput
  :: Html -> String -> MomentIO (Event String)
onInput ~(Html nodes) selector =
  do (input,fireInput) <- newEvent
     eventListener <-
       liftIO (eventListenerNew
                 ((\ev ->
                     do Just target <- getTarget ev
                        getValue (castToHTMLInputElement target) >>= fireInput) :: UIEvent -> IO ()))
     do initialNodes <- valueBLater nodes
        liftIOLater
          (do for_ initialNodes
                   (\node ->
                      do Just nodeList <-
                           querySelectorAll (castToElement (id node))
                                            selector
                         mnode <- item nodeList 0 -- TODO
                         case mnode of
                           Nothing -> putStrLn "Selector found no elements"
                           Just target ->
                             addEventListener (castToElement target)
                                              ("input" :: String)
                                              (Just eventListener)
                                              False))
     return (fmap (fromMaybe "") input)

onClick :: Html -> String -> MomentIO (Event ())
onClick ~(Html nodes) selector =
  do (click,fireClick) <- newEvent
     eventListener <-
       liftIO (eventListenerNew ((\_ -> fireClick ()) :: UIEvent -> IO ()))
     do initialNodes <- valueBLater nodes
        liftIOLater
          (do for_ initialNodes
                   (\node ->
                      do Just nodeList <-
                           querySelectorAll (castToElement (id node))
                                            (":scope " <> selector)
                         mnode <- item nodeList 0 -- TODO
                         case mnode of
                           Nothing -> putStrLn "Selector found no elements"
                           Just target ->
                             addEventListener (castToElement target)
                                              ("click" :: String)
                                              (Just eventListener)
                                              False))
     return click

joinHtml :: Behavior (ReactiveHtml a) -> ReactiveHtml ()
joinHtml bhtml =
  ReactiveHtml $
  do (sample,handle) <- newEvent
     bChanged <- changes bhtml
     reactimate' (fmap (fmap handle) bChanged)
     laterNodes <- execute (fmap (\(ReactiveHtml m) -> m) sample)
     initialHtml <- valueB bhtml >>= \(ReactiveHtml m) -> fmap snd m
     nodes <- switchB initialHtml (fmap snd laterNodes)
     return ((),nodes)

html :: Html -> ReactiveHtml ()
html (Html b) = ReactiveHtml (return ((),b))

runReactiveHtml :: MomentIO Html -> IO ()
runReactiveHtml app =
  do Just document <- currentDocument
     Just body <- getBody document
     n <-
       compile (do Html nodes <- app
                   initialNodes <- valueB nodes
                   nodesChanged <- changes nodes
                   liftIO (traverse_ (appendChild body . Just . id) initialNodes))
     actuate n
