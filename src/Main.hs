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
  import Data.Kind
  import Data.Monoid
  import Data.Singletons
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
    MkEvent :: (Show (EventValue e), Show (TermValue k)) => EventValue e -> TermValue k -> Event e k

  deriving instance Show (Event e t)

  data SomeEvent :: Type where
    MkSomeEvent ::  Sing e -> Sing t -> Event e t -> SomeEvent 

  fromEvent :: (SingI e, SingI t) => Event e t -> SomeEvent
  fromEvent = MkSomeEvent sing sing

  type instance EventValue Simple = Name

  type instance TermValue None  = ()
  type instance TermValue At    = ZonedTime
  type instance TermValue From  = (ZonedTime, DiffTime)
  type instance TermValue To    = (ZonedTime, DiffTime)

  event :: Name -> Event Simple None
  event n = MkEvent n ()

  appoint :: ZonedTime -> Event a None -> Event a At
  appoint z (MkEvent a _) = MkEvent a z

  showSE :: SomeEvent -> String
  showSE (MkSomeEvent e t ev) = "MkSomeEvent " <> show (fromSing e) <> " " <> show (fromSing t) <> " " <> show ev
  
  instance Show SomeEvent where
    showsPrec i (MkSomeEvent e t ev) = 
        showString "MkSomeEvent " . 
        showsPrec i (fromSing e) . showString " " . 
        showsPrec i (fromSing t) . showString " " .
        showParen True (showsPrec (i + 1) ev)

  main :: IO ()
  main = do
    let ev = event "Event 1"
    putStrLn $ show ev
    ap <- flip appoint ev <$> getZonedTime
    putStrLn $ show ap
    let evs = [fromEvent ev, fromEvent ap]
    putStrLn $ show evs
