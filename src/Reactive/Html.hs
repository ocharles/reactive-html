{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactive.Html
       ( -- * Running applications
         runHtml, Reactive, ReactiveNodes, render,

         -- * Building reactive HTML
         ReactiveHtml, text_, joinHtml, html,

         -- ** Handling Events
         on, onClick, onInput,

         -- ** HTML 5 Elements
         a_, abbr_, address_, area_, article_, aside_, audio_, b_, base_, bdi_,
         bdo_, blockquote_, body_, br_, button_, canvas_, caption_, cite_, code_,
         col_, colgroup_, command_, datalist_, dd_, del_, details_, dfn_, div_,
         dl_, dt_, em_, embed_, fieldset_, figcaption_, figure_, footer_, form_,
         h1_, h2_, h3_, h4_, h5_, h6_, head_, header_, hgroup_, hr_, html_, i_,
         iframe_, img_, input_, ins_, kbd_, keygen_, label_, legend_, li_, link_,
         map_, mark_, menu_, meta_, meter_, nav_, noscript_, object_, ol_,
         optgroup_, option_, output_, p_, param_, pre_, progress_, q_, rp_, rt_,
         ruby_, s_, samp_, script_, section_, select_, small_, source_, span_,
         strong_, style_, sub_, summary_, sup_, table_, tbody_, td_, textarea_,
         tfoot_, th_, thead_, time_, title_, tr_, track_, u_, ul_, var_, video_,
         wbr_,

         -- ** HTML 5 Attributes
         id_, value_,

         -- ** DOM API
         makeElement, Term(..), VoidTerm(..), Attribute(..))
       where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT) -- TODO Use strict state
import Control.Monad.Writer (tell)
import Data.Foldable (traverse_,for_)
import Data.Maybe
import qualified Data.Patch as Patch
import Data.String (IsString(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.CharacterData (setData)
import GHCJS.DOM.Document (createElement, createTextNode, getBody)
import GHCJS.DOM.Element (castToElement, querySelectorAll, setAttribute)
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
import JavaScript.Web.AnimationFrame
import Reactive.Banana
import Reactive.Banana.Frameworks

foreign import javascript unsafe "$1 === $2"
  js_eqRef :: Prim.JSVal -> Prim.JSVal -> Bool

--------------------------------------------------------------------------------
data ReactiveNodes =
  ReactiveNodes {rnNodes :: Behavior (Vector EqNode)
                ,rnStale :: Event ()}

instance Monoid ReactiveNodes where
  mempty =
    ReactiveNodes (pure mempty)
                  never
  ReactiveNodes a x `mappend` ReactiveNodes b y =
    ReactiveNodes (liftA2 mappend a b)
                  (unionWith const x y)

--------------------------------------------------------------------------------
-- | DOM nodes with a notion of equality (based on pointer equality of the
-- underlying JavaScript object).
newtype EqNode = EqNode Node
  deriving (DOM.IsNode, DOM.IsEventTarget, DOM.IsGObject, ToJSVal, FromJSVal)

instance Eq EqNode where
  EqNode l == EqNode r =
    case (pToJSVal l,pToJSVal r) of
      (a,b) -> js_eqRef a b

--------------------------------------------------------------------------------
-- | A writer monad over DOM nodes that can change over time.
newtype ReactiveHtml a =
  ReactiveHtml (WriterT ReactiveNodes MomentIO a)
  deriving (Functor, Applicative, Monad, MonadFix)

liftMomentIO :: MomentIO a -> ReactiveHtml a
liftMomentIO = ReactiveHtml . lift

instance Monoid a => Monoid (ReactiveHtml a) where
  mempty = return mempty
  mappend = liftA2 mappend

-- | Create a text DOM node.
instance a ~ () => IsString (ReactiveHtml a) where
  fromString str =
    ReactiveHtml $
    do Just document <- liftIO currentDocument
       Just tNode <-
         liftIO (createTextNode document str)
       tell (ReactiveNodes (pure (pure (EqNode (castToNode tNode)))) never) -- TODO Fire once?

-- | Create a text DOM node whose contents can vary.
text_ :: (?animationFrame :: Event ()) => Behavior String -> ReactiveHtml ()
text_ text =
  do liftMomentIO
       (do textNode <- liftIO newEmptyTextNode
           currentValue <-
             stepper "" (text <@ ?animationFrame)
           reactimate
             (fmap (setText textNode)
                   (filterApply ((/=) <$> currentValue)
                                (text <@ ?animationFrame)))
           requestAnimationFrame <- void <$> changes text
           return (ReactiveNodes (pure (pure (EqNode (castToNode textNode))))
                                 requestAnimationFrame)) >>=
       ReactiveHtml . tell
  where newEmptyTextNode =
          do Just document <- currentDocument
             Just node <-
               createTextNode document ""
             return node
        setText node t = setData node (Just t)

--------------------------------------------------------------------------------
newtype Attribute = Attribute (DOM.Element -> MomentIO (Event ()))

applyAttributes :: DOM.Element -> [Attribute] -> MomentIO (Event ())
applyAttributes el =
  fmap (foldl (unionWith const) never) . mapM (\(Attribute f) -> f el)

id_ :: Reactive => Behavior String -> Attribute
id_ v =
  Attribute $
  \el ->
    do let update x =
             setAttribute el "id" x
       valueChanged <- changes v
       reactimate (fmap update (v <@ ?animationFrame))
       return (void valueChanged)

value_ :: Reactive => Behavior String -> Attribute
value_ v =
  Attribute $
  \el ->
    do (input,fixInput) <- newEvent
       liftIOLater
         (do eventListener <-
               eventListenerNew (const (fixInput ()) :: UIEvent -> IO ())
             addEventListener el
                              "input"
                              (Just eventListener)
                              False)
       reactimate
         (fmap (setValue (castToHTMLInputElement el) . Just)
               (v <@ ?animationFrame))
       return (void input)

--------------------------------------------------------------------------------
class Term_ args r | r -> args where
  mkTerm :: (?animationFrame :: Event ()) => String -> args -> r

instance (html ~ ReactiveHtml (), b ~ ()) => Term_ html (ReactiveHtml b) where
  mkTerm el = makeElement el []

instance (attributes ~ [Attribute],children ~ ReactiveHtml a,html ~ ReactiveHtml ()) => Term_ attributes (children -> html) where
  mkTerm el = makeElement el

type Term args html = (Term_ args html, ?animationFrame :: Event ())

a_ :: Term args html => args -> html
a_ = mkTerm "a"

abbr_ :: Term args html => args -> html
abbr_ = mkTerm "abbr"

address_ :: Term args html => args -> html
address_ = mkTerm "address"

article_ :: Term args html => args -> html
article_ = mkTerm "article"

aside_ :: Term args html => args -> html
aside_ = mkTerm "aside"

audio_ :: Term args html => args -> html
audio_ = mkTerm "audio"

b_ :: Term args html => args -> html
b_ = mkTerm "b"

bdi_ :: Term args html => args -> html
bdi_ = mkTerm "bdi"

bdo_ :: Term args html => args -> html
bdo_ = mkTerm "bdo"

blockquote_ :: Term args html => args -> html
blockquote_ = mkTerm "blockquote"

body_ :: Term args html => args -> html
body_ = mkTerm "body"

button_ :: Term args html => args -> html
button_ = mkTerm "button"

canvas_ :: Term args html => args -> html
canvas_ = mkTerm "canvas"

caption_ :: Term args html => args -> html
caption_ = mkTerm "caption"

cite_ :: Term args html => args -> html
cite_ = mkTerm "cite"

code_ :: Term args html => args -> html
code_ = mkTerm "code"

colgroup_ :: Term args html => args -> html
colgroup_ = mkTerm "colgroup"

datalist_ :: Term args html => args -> html
datalist_ = mkTerm "datalist"

dd_ :: Term args html => args -> html
dd_ = mkTerm "dd"

del_ :: Term args html => args -> html
del_ = mkTerm "del"

details_ :: Term args html => args -> html
details_ = mkTerm "details"

dfn_ :: Term args html => args -> html
dfn_ = mkTerm "dfn"

div_ :: Term args html => args -> html
div_ = mkTerm "div"

dl_ :: Term args html => args -> html
dl_ = mkTerm "dl"

dt_ :: Term args html => args -> html
dt_ = mkTerm "dt"

em_ :: Term args html => args -> html
em_ = mkTerm "em"

fieldset_ :: Term args html => args -> html
fieldset_ = mkTerm "fieldset"

figcaption_ :: Term args html => args -> html
figcaption_ = mkTerm "figcaption"

figure_ :: Term args html => args -> html
figure_ = mkTerm "figure"

footer_ :: Term args html => args -> html
footer_ = mkTerm "footer"

form_ :: Term args html => args -> html
form_ = mkTerm "form"

h1_ :: Term args html => args -> html
h1_ = mkTerm "h1"

h2_ :: Term args html => args -> html
h2_ = mkTerm "h2"

h3_ :: Term args html => args -> html
h3_ = mkTerm "h3"

h4_ :: Term args html => args -> html
h4_ = mkTerm "h4"

h5_ :: Term args html => args -> html
h5_ = mkTerm "h5"

h6_ :: Term args html => args -> html
h6_ = mkTerm "h6"

head_ :: Term args html => args -> html
head_ = mkTerm "head"

header_ :: Term args html => args -> html
header_ = mkTerm "header"

hgroup_ :: Term args html => args -> html
hgroup_ = mkTerm "hgroup"

html_ :: Term args html => args -> html
html_ = mkTerm "html"

i_ :: Term args html => args -> html
i_ = mkTerm "i"

iframe_ :: Term args html => args -> html
iframe_ = mkTerm "iframe"

ins_ :: Term args html => args -> html
ins_ = mkTerm "ins"

kbd_ :: Term args html => args -> html
kbd_ = mkTerm "kbd"

label_ :: Term args html => args -> html
label_ = mkTerm "label"

legend_ :: Term args html => args -> html
legend_ = mkTerm "legend"

li_ :: Term args html => args -> html
li_ = mkTerm "li"

map_ :: Term args html => args -> html
map_ = mkTerm "map"

mark_ :: Term args html => args -> html
mark_ = mkTerm "mark"

menu_ :: Term args html => args -> html
menu_ = mkTerm "menu"

meter_ :: Term args html => args -> html
meter_ = mkTerm "meter"

nav_ :: Term args html => args -> html
nav_ = mkTerm "nav"

noscript_ :: Term args html => args -> html
noscript_ = mkTerm "noscript"

object_ :: Term args html => args -> html
object_ = mkTerm "object"

ol_ :: Term args html => args -> html
ol_ = mkTerm "ol"

optgroup_ :: Term args html => args -> html
optgroup_ = mkTerm "optgroup"

option_ :: Term args html => args -> html
option_ = mkTerm "option"

output_ :: Term args html => args -> html
output_ = mkTerm "output"

p_ :: Term args html => args -> html
p_ = mkTerm "p"

pre_ :: Term args html => args -> html
pre_ = mkTerm "pre"

progress_ :: Term args html => args -> html
progress_ = mkTerm "progress"

q_ :: Term args html => args -> html
q_ = mkTerm "q"

rp_ :: Term args html => args -> html
rp_ = mkTerm "rp"

rt_ :: Term args html => args -> html
rt_ = mkTerm "rt"

ruby_ :: Term args html => args -> html
ruby_ = mkTerm "ruby"

s_ :: Term args html => args -> html
s_ = mkTerm "s"

samp_ :: Term args html => args -> html
samp_ = mkTerm "samp"

script_ :: Term args html => args -> html
script_ = mkTerm "script"

section_ :: Term args html => args -> html
section_ = mkTerm "section"

select_ :: Term args html => args -> html
select_ = mkTerm "select"

small_ :: Term args html => args -> html
small_ = mkTerm "small"

span_ :: Term args html => args -> html
span_ = mkTerm "span"

strong_ :: Term args html => args -> html
strong_ = mkTerm "strong"

style_ :: Term args html => args -> html
style_ = mkTerm "style"

sub_ :: Term args html => args -> html
sub_ = mkTerm "sub"

summary_ :: Term args html => args -> html
summary_ = mkTerm "summary"

sup_ :: Term args html => args -> html
sup_ = mkTerm "sup"

table_ :: Term args html => args -> html
table_ = mkTerm "table"

tbody_ :: Term args html => args -> html
tbody_ = mkTerm "tbody"

td_ :: Term args html => args -> html
td_ = mkTerm "td"

textarea_ :: Term args html => args -> html
textarea_ = mkTerm "textarea"

tfoot_ :: Term args html => args -> html
tfoot_ = mkTerm "tfoot"

th_ :: Term args html => args -> html
th_ = mkTerm "th"

thead_ :: Term args html => args -> html
thead_ = mkTerm "thead"

time_ :: Term args html => args -> html
time_ = mkTerm "time"

title_ :: Term args html => args -> html
title_ = mkTerm "title"

tr_ :: Term args html => args -> html
tr_ = mkTerm "tr"

u_ :: Term args html => args -> html
u_ = mkTerm "u"

ul_ :: Term args html => args -> html
ul_ = mkTerm "ul"

var_ :: Term args html => args -> html
var_ = mkTerm "var"

video_ :: Term args html => args -> html
video_ = mkTerm "video"

--------------------------------------------------------------------------------
class VoidTerm r where
  mkVoidTerm :: String -> r

instance b ~ () => VoidTerm (ReactiveHtml b) where
  mkVoidTerm el = makeVoidElement el []

instance (attributes ~ [Attribute], html ~ ReactiveHtml ()) => VoidTerm (attributes -> html) where
  mkVoidTerm el = makeVoidElement el

makeVoidElement
  :: String -> [Attribute] -> ReactiveHtml ()
makeVoidElement el attrs =
  ReactiveHtml $
  do Just divElement <-
       liftIO (do Just document <- currentDocument
                  createElement document
                                (Just el :: Maybe String))
     attributesChanged <-
       lift (applyAttributes divElement attrs)
     tell (ReactiveNodes (pure (pure (EqNode (castToNode divElement))))
                         attributesChanged)

area_ :: VoidTerm html => html
area_ = mkVoidTerm "area"

base_ :: VoidTerm html => html
base_ = mkVoidTerm "base"

br_ :: VoidTerm html => html
br_ = mkVoidTerm "br"

col_ :: VoidTerm html => html
col_ = mkVoidTerm "col"

command_ :: VoidTerm html => html
command_ = mkVoidTerm "command"

embed_ :: VoidTerm html => html
embed_ = mkVoidTerm "embed"

hr_ :: VoidTerm html => html
hr_ = mkVoidTerm "hr"

img_ :: VoidTerm html => html
img_ = mkVoidTerm "img"

input_ :: VoidTerm html => html
input_ = mkVoidTerm "input"

keygen_ :: VoidTerm html => html
keygen_ = mkVoidTerm "keygen"

link_ :: VoidTerm html => html
link_ = mkVoidTerm "link"

meta_ :: VoidTerm html => html
meta_ = mkVoidTerm "meta"

param_ :: VoidTerm html => html
param_ = mkVoidTerm "param"

source_ :: VoidTerm html => html
source_ = mkVoidTerm "source"

track_ :: VoidTerm html => html
track_ = mkVoidTerm "track"

wbr_ :: VoidTerm html => html
wbr_ = mkVoidTerm "wbr"

--------------------------------------------------------------------------------
makeElement
  :: (?animationFrame :: Event ()) => String -> [Attribute] -> ReactiveHtml a -> ReactiveHtml ()
makeElement el attrs (ReactiveHtml mkChildren) =
  liftMomentIO
    (do ReactiveNodes children childrenRaf <- execWriterT mkChildren
        element <- liftIO newDivElement
        attributesChanged <-
          applyAttributes element attrs
        currentChildren <-
          stepper mempty (children <@ ?animationFrame)
        reactimate
          (fmap (setChildren element) currentChildren <@>
           (filterApply ((/=) <$> currentChildren)
                        (children <@ ?animationFrame)))
        requestAnimationFrame <- void <$> changes children
        return (ReactiveNodes
                  (pure (pure (EqNode (castToNode element))))
                  (foldl (unionWith const)
                         never
                         [requestAnimationFrame,childrenRaf,attributesChanged]))) >>=
  ReactiveHtml . tell
  where newDivElement =
          do Just document <- currentDocument
             Just node <-
               createElement document
                             (Just el)
             return node
        setChildren element old new =
          let edits =
                Patch.toList (Patch.diff old new)
          in for_ edits
                  (\edit ->
                     case edit of
                       Patch.Insert n node ->
                         insertBefore element
                                      (Just node)
                                      (old V.!? n)
                       Patch.Replace _ oldNode newNode ->
                         replaceChild element
                                      (Just newNode)
                                      (Just oldNode)
                       Patch.Delete _ node ->
                         removeChild element
                                     (Just node))

--------------------------------------------------------------------------------
-- TODO Would much rather have DOM.Event here, but for some reason I am unable
-- to later cast that back to UIEvent.
-- TODO Do we need to call eventListenerRelease here?

on :: Reactive => String -> ReactiveNodes -> String -> MomentIO (Event DOM.UIEvent)
on eventName ~(ReactiveNodes nodes _) selector =
  do (evOccured,fireEvent) <- newEvent
     eventListener <-
       liftIO (eventListenerNew fireEvent)
     reactimate
       (fmap (rebind eventListener)
             (nodes <@ ?rebindEventHandlers))
     return evOccured
  where rebind eventListener =
          traverse_ (\node ->
                       do Just nodeList <-
                            querySelectorAll (castToElement (id node))
                                             selector
                          mnode <-
                            item nodeList 0 -- TODO Consider all results
                          case mnode of
                            Nothing -> return ()
                            Just target ->
                              addEventListener (castToElement target)
                                               eventName
                                               (Just eventListener)
                                               False)
onInput
  :: Reactive
  => ReactiveNodes -> String -> MomentIO (Event String)
onInput view selector =
  do input <- on "input" view selector
     execute (fmap (\ev ->
                      liftIO (do Just target <- getTarget ev
                                 fmap (fromMaybe "")
                                      (getValue (castToHTMLInputElement target))))
                   input)

onClick
  :: Reactive
  => ReactiveNodes -> String -> MomentIO (Event ())
onClick view selector = fmap void (on "click" view selector)

--------------------------------------------------------------------------------
type Reactive = (?animationFrame :: Event (), ?rebindEventHandlers :: Event ())

render :: ReactiveHtml a -> MomentIO ReactiveNodes
render (ReactiveHtml mk) = execWriterT mk

joinHtml :: Reactive => Behavior (ReactiveHtml a) -> ReactiveHtml ()
joinHtml bhtml =
  ReactiveHtml $
  do (sample,handle) <- lift newEvent
     bChanged <- lift (changes bhtml)
     lift (reactimate' (fmap (fmap handle) bChanged))
     laterNodes <-
       lift (execute (fmap (\(ReactiveHtml m) -> execWriterT m) sample))
     ReactiveNodes initialHtml mutated <-
       lift (valueB bhtml) >>=
       \(ReactiveHtml w) -> lift (execWriterT w)
     nodes <-
       lift (switchB initialHtml (fmap rnNodes laterNodes))
     raf <-
       lift (switchE (fmap rnStale laterNodes))
     tell (ReactiveNodes nodes
                         (unionWith const mutated raf))

html :: ReactiveNodes -> ReactiveHtml ()
html = ReactiveHtml . tell

--------------------------------------------------------------------------------
runHtml
  :: (Reactive => MomentIO ReactiveNodes) -> IO ()
runHtml app =
  do (rafAH,animationFrameArrived) <- newAddHandler
     (rafPostAH,animationFrameDone) <- newAddHandler
     let dispatch = do _ <- waitForAnimationFrame
                       animationFrameArrived ()
                       animationFrameDone ()
     network <-
       compile (do animationFrame <- fromAddHandler rafAH
                   animationFrameComplete <- fromAddHandler rafPostAH
                   ReactiveNodes nodes raf <-
                     let ?animationFrame = animationFrame
                         ?rebindEventHandlers = animationFrameComplete
                     in app
                   currentNodes <-
                     stepper mempty (nodes <@ animationFrame)
                   reactimate
                     (fmap sink
                           (filterApply ((/=) <$> currentNodes)
                                        (nodes <@ animationFrame)))
                   reactimate (dispatch <$ raf))
     actuate network
     dispatch
  where sink nodes =
          do Just document <- currentDocument
             Just body <- getBody document
             traverse_ (appendChild body . Just) nodes
