{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, LambdaCase, PolyKinds #-}

module Data.Aeson.TypeScript.Recursive (
  getTransitiveClosure
  ) where

import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Containers.ListUtils (nubOrd)


getTransitiveClosure :: [TSType] -> [TSType]
getTransitiveClosure initialTypes = fix (\loop items -> let items' = nubOrd (items ++ concatMap getMore items) in
                                            if | items' == items -> items
                                               | otherwise -> loop items'
                                        ) initialTypes
  where getMore :: TSType -> [TSType]
        getMore (TSType x) = getParentTypes x
