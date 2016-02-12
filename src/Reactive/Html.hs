{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactive.Html
       ( -- * Running applications
         runHtml, Html, render,

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
import Reactive.Banana
import Reactive.Banana.Frameworks

foreign import javascript unsafe "$1 === $2"
  js_eqRef :: Prim.JSVal -> Prim.JSVal -> Bool

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

-- | Create a text DOM node.
instance a ~ () => IsString (ReactiveHtml a) where
  fromString str =
    ReactiveHtml $
    do Just document <- liftIO currentDocument
       Just tNode <- liftIO (createTextNode document str)
       return ((),pure (pure (EqNode (castToNode tNode))))

-- | Create a text DOM node whose contents can vary.
text_ :: Behavior String -> ReactiveHtml ()
text_ contents =
  ReactiveHtml $
  do Just textNode <-
       liftIO (do Just document <- currentDocument
                  createTextNode document "")
     do initialText <- valueBLater contents
        liftIOLater (setData textNode (Just initialText))
     do contentsChanged <- changes contents
        reactimate' (fmap (fmap (setData textNode . Just)) contentsChanged)
     return ((),pure (pure (EqNode (castToNode textNode))))

--------------------------------------------------------------------------------
newtype Attribute = Attribute (DOM.Element -> MomentIO ())

applyAttributes :: DOM.Element -> [Attribute] -> MomentIO ()
applyAttributes el = mapM_ (\(Attribute f) -> f el)

id_ :: Behavior String -> Attribute
id_ v =
  Attribute $
  \el ->
    do let update = setAttribute el "id"
       valueBLater v >>= liftIOLater . update
       valueChanged <- changes v
       reactimate' (fmap (fmap update) valueChanged)

value_ :: Behavior String -> Attribute
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
       initialValue <- valueB v
       liftIO (setValue (castToHTMLInputElement el)
                        (Just initialValue))
       valueChanged <- changes v
       reactimate' (fmap (fmap (setValue (castToHTMLInputElement el) . Just)) valueChanged)
       reactimate
         (fmap (setValue (castToHTMLInputElement el) . Just)
               (v <@ input))

--------------------------------------------------------------------------------
class Term args r | r -> args where
  mkTerm :: String -> args -> r

instance (html ~ ReactiveHtml (), b ~ ()) => Term html (ReactiveHtml b) where
  mkTerm el = makeElement el []

instance (attributes ~ [Attribute],children ~ ReactiveHtml a,html ~ ReactiveHtml ()) => Term attributes (children -> html) where
  mkTerm el = makeElement el

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
     applyAttributes divElement attrs
     return ((),pure (pure (EqNode (castToNode divElement))))

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
  :: String -> [Attribute] -> ReactiveHtml a -> ReactiveHtml ()
makeElement el attrs (ReactiveHtml mkChildren) =
  ReactiveHtml $
  do Just divElement <-
       liftIO (do Just document <- currentDocument
                  createElement document
                                (Just el :: Maybe String))
     applyAttributes divElement attrs
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

--------------------------------------------------------------------------------
-- TODO Would much rather have DOM.Event here, but for some reason I am unable
-- to later cast that back to UIEvent.
-- TODO Do we need to call eventListenerRelease here?
on :: String -> Html -> String -> MomentIO (Event DOM.UIEvent)
on eventName ~(Html nodes) selector =
  do (evOccured,fireEvent) <- newEvent
     eventListener <- liftIO (eventListenerNew fireEvent)
     do initialNodes <- valueBLater nodes
        liftIOLater
          (do for_ initialNodes
                   (\node ->
                      do Just nodeList <-
                           querySelectorAll (castToElement (id node))
                                            selector
                         mnode <- item nodeList 0 -- TODO Consider all results
                         case mnode of
                           Nothing -> putStrLn "Selector found no elements"
                           Just target ->
                             addEventListener (castToElement target)
                                              eventName
                                              (Just eventListener)
                                              False))
     return evOccured

onInput
  :: Html -> String -> MomentIO (Event String)
onInput view selector =
  do input <- on "input" view selector
     execute (fmap (\ev ->
                      liftIO (do Just target <- getTarget ev
                                 fmap (fromMaybe "")
                                      (getValue (castToHTMLInputElement target))))
                   input)

onClick :: Html -> String -> MomentIO (Event ())
onClick view selector = fmap void (on "click" view selector)

--------------------------------------------------------------------------------
newtype Html =
  Html (Behavior (Vector EqNode))

instance Monoid Html where
  mempty = Html (pure mempty)
  Html l `mappend` Html r = Html (liftA2 mappend l r)

render :: ReactiveHtml a -> MomentIO Html
render (ReactiveHtml mk) = fmap (Html . snd) mk

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

--------------------------------------------------------------------------------
runHtml :: MomentIO Html -> IO ()
runHtml app =
  do Just document <- currentDocument
     Just body <- getBody document
     n <-
       compile (do Html nodes <- app
                   initialNodes <- valueB nodes
                   nodesChanged <- changes nodes
                   liftIO (traverse_ (appendChild body . Just . id) initialNodes))
     actuate n
