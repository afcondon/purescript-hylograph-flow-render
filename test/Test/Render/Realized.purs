-- | A HATS tree read in node, with no browser: every fold run over its own
-- | data, so that what is left is the elements an interpreter would make,
-- | with their attributes and their behaviours.
-- |
-- | `hylograph-selection` has interpreters that turn a tree into words
-- | (`Hylograph.Interpreter.English`), a diagram (`Mermaid`) and code
-- | (`SemiQuine.TreeToCode`); each describes a fold and none runs one over
-- | every datum, which is what counting marks needs. So this is here.
module Test.Render.Realized
  ( Realized(..)
  , realize
  , attribute
  , classes
  , hasClass
  , children
  , everything
  , clicks
  , enters
  , leaves
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect (Effect)
import Hylograph.HATS (Attr(..), ElementType, Enumeration(..), ThunkedBehavior(..), Tree(..), runSomeFold)

newtype Realized = Realized
  { element :: ElementType
  , attributes :: Array { name :: String, value :: String }
  , behaviors :: Array ThunkedBehavior
  , children :: Array Realized
  }

-- | The elements a tree stands for, in order. A fold over an array (which
-- | is what `forEach` makes) becomes its template's elements, one datum
-- | after another; a fold over a tree is not followed, and is said.
realize :: Tree -> Either String (Array Realized)
realize = case _ of
  Empty -> Right []
  Elem e -> traverse realize e.children <#> \within ->
    [ Realized
        { element: e.elemType
        , attributes: e.attrs <#> case _ of
            StaticAttr name value -> { name, value }
            ThunkedAttr name value -> { name, value: value unit }
        , behaviors: e.behaviors
        , children: Array.concat within
        }
    ]
  MkFold someFold -> runSomeFold someFold \spec -> case spec.enumerate of
    FromArray items -> Array.concat <$> traverse (realize <<< spec.template) items
    WithContext items -> Array.concat <$> traverse (realize <<< spec.template <<< _.datum) items
    FromTree _ -> Left ("the fold " <> show spec.name <> " is over a tree, which this reading does not follow")

-- | An attribute's value; the last said, as in a DOM.
attribute :: String -> Realized -> Maybe String
attribute name (Realized r) = _.value <$> Array.last (Array.filter (\a -> a.name == name) r.attributes)

classes :: Realized -> Array String
classes r = case attribute "class" r of
  Just value -> Array.filter (_ /= "") (split (Pattern " ") value)
  Nothing -> []

hasClass :: String -> Realized -> Boolean
hasClass name r = Array.elem name (classes r)

children :: Realized -> Array Realized
children (Realized r) = r.children

-- | An element and all under it.
everything :: Realized -> Array Realized
everything r = [ r ] <> Array.concatMap everything (children r)

clicks :: Realized -> Array (Effect Unit)
clicks (Realized r) = r.behaviors # Array.mapMaybe case _ of
  ThunkedClick handler -> Just (handler unit)
  _ -> Nothing

enters :: Realized -> Array (Effect Unit)
enters (Realized r) = r.behaviors # Array.mapMaybe case _ of
  ThunkedMouseEnter handler -> Just (handler unit)
  _ -> Nothing

leaves :: Realized -> Array (Effect Unit)
leaves (Realized r) = r.behaviors # Array.mapMaybe case _ of
  ThunkedMouseLeave handler -> Just (handler unit)
  _ -> Nothing
