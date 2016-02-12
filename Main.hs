{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.Fix (mfix)
import Data.Monoid
import ReactiveHtml
import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO, execute)

data ToDoItem =
  ToDoItem {toDoItemView :: ReactiveHtml ()
           ,toDoItemRemove :: Event ()}

newToDoItem
  :: Behavior String -> MomentIO ToDoItem
newToDoItem itemText =
  do view <- render (div_ (text_ itemText <> " " <> button_ "X"))
     toDoItemRemove <- onClick view "button"
     return ToDoItem {toDoItemView = html view
                     ,..}

app :: MomentIO Html
app =
  mfix $
  \view ->
    mdo newToDoText <-
          do changeToDoInput <- onInput view "input"
             accumB ""
                    (unions [const <$> changeToDoInput
                            ,const "" <$ addNewToDoItem])
        addNewToDoItem <-
          do clickAddItem <- onClick view "button"
             execute (fmap (\initialText -> newToDoItem (pure initialText))
                           (newToDoText <@ clickAddItem))
        (toDoItemsChanged,toDoItems) <-
          mapAccum []
                   (fmap (\update items -> (update items,update items))
                         (unions [fmap (:) addNewToDoItem,deleteToDoItem]))
        deleteToDoItem <-
          switchE (fmap (unions .
                         zipWith (\i toDoItem ->
                                    deleteAt i <$ toDoItemRemove toDoItem)
                                 [0 ..])
                        toDoItemsChanged)
        render $
          div_ $
          do div_ $
               do "To do: " <> input_ [value_ newToDoText]
                  button_ "Add"
             ul_ (joinHtml (fmap (mapM_ (li_ . toDoItemView)) toDoItems))

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt j (x:xs)
  | j < 0 = xs
  | j > length xs = xs
  | j == 0 = xs
  | otherwise = x : deleteAt (j - 1) xs

main :: IO ()
main = runHtml app
