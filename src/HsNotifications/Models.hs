{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HsNotifications.Models where

import Control.Monad ((>=>))
import DBus (IsVariant(..), Variant)
import Data.Int (Int32)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8, Word32)

import qualified Data.Text as T
import qualified GI.Gtk as Gtk


-- | The State passed through the application.
data AppState
    = AppState
        { appNotificationQueue :: NotificationQueue     -- ^ The Add/Replace Queue from DBus Messages
        , appEventQueue :: EventQueue                   -- ^ The Queue of GUI->DBus Signals to Send
        , appWindowList :: [(Notification, Gtk.Window)] -- ^ The List of Rendered Windows in top-to-bottom order.
        , appRootPosition :: (Int32, Int32)             -- ^ The base position to place Notifications
        , appNextPosition :: (Int32, Int32)             -- ^ The position for the next Notification
        , appNextNotificationID :: NotificationID       -- ^ The ID for the next Notification
        }


-- | A Notification is received by DBus and used to build a `Gtk.Window`.
data Notification
    = Notification
        { nTitle :: T.Text
        , nBody :: T.Text
        , nUrgency :: Urgency
        , nID :: NotificationID
        , nActions :: [(ActionKey, T.Text)]
        , nExpirationTime :: Maybe UTCTime
        }

-- | A Sort-of-Unique Identifier for a Notification. IDs are allowed to
-- repeat if we have reach the overflow limit for `Word32`.
newtype NotificationID
    = NotificationID
        { fromNotificationID :: Word32
        } deriving (Eq)

-- | Generate the next NotificationID, looping back to 0 if we have reached
-- the maximum possible value.
nextID :: NotificationID -> NotificationID
nextID (NotificationID w) =
    if w == maxBound then
        NotificationID minBound
    else
        NotificationID $ succ w

-- | DBus' Urgency Level for `Notification`s.
data Urgency
    = Low
    | Normal
    | Critical
    deriving (Eq)

instance IsVariant Urgency where
    toVariant a = toVariant $ case a of
        Low -> 0 :: Word8
        Normal -> 1
        Critical -> 2
    fromVariant = (fromVariant :: Variant -> Maybe Word8) >=> \case
        0 -> Just Low
        1 -> Just Normal
        2 -> Just Critical
        _ -> Nothing

-- | Identifiers for a notification's potential actions
newtype ActionKey
    = ActionKey
        { fromActionKey :: T.Text
        } deriving (Eq)

isDefaultAction :: (ActionKey, T.Text) -> Bool
isDefaultAction =
    (== "default") . fromActionKey . fst


-- | The Queue generated by DBus when receiving `Notify` calls.
--
-- Newest notifications at the front.
type NotificationQueue
    = [ (Notification, QueueRequest) ]

-- | Whether DBus told us to Add or Replace the given `Notification`.
data QueueRequest
    = Add
    | Replace


type EventQueue
    = [ (NotificationID, AppEvent) ]

data AppEvent
    = NotificationClosed ReasonClosed
    | ActionTriggered ActionKey

-- | The Queue consumed by DBus for sending `NotificationClosed` Signals
type RemovalQueue
    = [ (NotificationID, ReasonClosed) ]

-- | The Reason a `Notification` Has Been Closed, Sent to the DBus Service
-- Clients.
data ReasonClosed
    = Expired
    | Dismissed
    | DBusCall
    | Other

-- | Transform the ReasonClosed type into the values expected/returned by DBus.
instance IsVariant ReasonClosed where
    toVariant a = toVariant $ case a of
        Expired -> 1 :: Word32
        Dismissed -> 2
        DBusCall -> 3
        Other -> 4
    fromVariant = (fromVariant :: Variant -> Maybe Word32) >=> \case
        1 -> Just Expired
        2 -> Just Dismissed
        3 -> Just DBusCall
        4 -> Just Other
        _ -> Nothing


type ActionQueue
    = [ (NotificationID, ActionKey) ]
