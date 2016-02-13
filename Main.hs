{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Monad.Fix (mfix)
import Data.Monoid
import Reactive.Html
import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO, execute, reactimate)

data ToDoItem =
  ToDoItem {toDoItemView :: ReactiveNodes
           ,toDoItemChange :: Event String
           ,toDoItemRemove :: Event ()}

newToDoItem
  :: Reactive => Behavior String -> MomentIO ToDoItem
newToDoItem itemText =
  mdo toDoItemRemove <-
        onClick toDoItemView "button"
      beginEditing <-
        on "dblclick" toDoItemView "#title"
      finishEditing <-
        on "blur" toDoItemView "#editing"
      changeToDoTitle <-
        onInput toDoItemView "#editing"
      editing <-
        stepper False
                (unionWith const
                           (False <$ finishEditing)
                           (True <$ beginEditing))
      editingName <-
        stepper "" (unionWith const (itemText <@ beginEditing) changeToDoTitle)
      toDoItemView <-
        render $
        div_ $
        do joinHtml $
             fmap (\editingInProgress ->
                     if editingInProgress
                        then input_ [id_ (pure "editing"),value_ editingName]
                        else do span_ [id_ (pure "title")]
                                      (text_ itemText)
                                " "
                                button_ "X")
                  editing
      return ToDoItem {toDoItemChange = editingName <@ finishEditing
                      ,..}

app :: Reactive => MomentIO ReactiveNodes
app =
  mfix $
  \view ->
    mdo newToDoText <-
          do changeToDoInput <-
               onInput view "input"
             stepper "" (unionWith const changeToDoInput ("" <$ addNewToDoItem))
        addNewToDoItem <-
          do clickAddItem <-
               onClick view "button"
             execute (fmap (\initialText ->
                              mdo itemText <-
                                    stepper initialText (toDoItemChange item)
                                  item <- newToDoItem itemText
                                  return item)
                           (newToDoText <@ clickAddItem))
        (toDoItemsChanged,toDoItems) <-
          mapAccum []
                   (fmap (\update items ->
                            (update items,update items))
                         (unions [fmap (:) addNewToDoItem,deleteToDoItem]))
        deleteToDoItem <-
          switchE (fmap (unions .
                         zipWith (\i toDoItem -> deleteAt i <$
                                                  toDoItemRemove toDoItem)
                                 [0 ..])
                        toDoItemsChanged)
        render $
          div_ $
          do div_ $
               do "To do: " <> input_ [value_ newToDoText]
                  button_ "Add"
             ul_ (joinHtml (fmap (mapM_ (li_ . html . toDoItemView)) toDoItems))
  where deleteAt _ [] = []
        deleteAt j (x:xs)
          | j <= 0 = xs
          | otherwise = x : deleteAt (j - 1) xs

main :: IO ()
main = runHtml app
