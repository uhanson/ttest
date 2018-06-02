{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Main where
  import Data.Singletons
  import Data.Kind
  import Data.Singletons.TH
  import Data.Time

  type Name = String

  $(singletons [d|
    data EventType
      = Simple 
      deriving (Show, Eq)
    |])

  $(singletons [d|
    data TermType
      = None
      | At
      | From
      | To
      deriving (Show, Eq)
    |])

  type family EventValue (k :: EventType) :: * 
  type family TermValue (k :: TermType) :: * 

  data Event :: EventType -> TermType -> Type where
    MkEvent :: EventValue e -> TermValue k -> Event e k
  
  deriving instance (Show (EventValue e), Show (TermValue t)) => Show (Event e t)

  type instance EventValue Simple = Name

  type instance TermValue None  = ()
  type instance TermValue At    = ZonedTime
  type instance TermValue From  = (ZonedTime, DiffTime)
  type instance TermValue To    = (ZonedTime, DiffTime)

  event :: Name -> Event Simple None
  event n = MkEvent n ()

  appoint :: ZonedTime -> Event a None -> Event a At
  appoint z (MkEvent a _) = MkEvent a z

  main :: IO ()
  main = do
    let ev = event "Event 1"
    putStrLn $ show ev
    ap <- flip appoint ev <$> getZonedTime
    putStrLn $ show ap
