{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Nagios.Config.EDSL.Serialize where

import Data.Maybe (catMaybes, mapMaybe)
import Data.List (intercalate)

import Nagios.Config.EDSL.Types

data Field = Field String String

field :: Encodable v => String -> v -> Maybe Field
field name value = Field name <$> encode value

lfield :: Encodable v => String -> [v] -> Maybe Field
lfield name value = Field name <$> encodeList value

class Serializable x where
    serialize :: x -> [Field]

instance Serializable Host where
    serialize Host{..} = catMaybes
        [field "use" hostUse]

instance Serializable Command where
    serialize Command{..} = catMaybes
        [ field "command_name" commandName
        , field "command_line" commandLine
        ]

instance Serializable TimePeriod where
    serialize TimePeriod{..} = catMaybes
        [ field "timeperiod_name" timePeriodName
        , field "alias" timePeriodAlias ] ++
        concatMap serialize timePeriodWeekdays

instance Serializable Contact where
    serialize Contact{..} = catMaybes
        [ field "use" contactUse
        , field "name" contactName
        , field "alias" contactAlias
        , lfield "contactgropus" contactGroups
        , field "host_notifications_enabled" contactHostNotificationsEnabled
        , field "service_notifications_enabled" contactServiceNotificationsEnabled
        , field "host_notification_period" contactHostNotificationPeriod
        , field "service_notification_period" contactServiceNotificationPeriod
        , lfield "host_notification_options" contactHostNotificationOptions
        , lfield "service_notification_options" contactServiceNotificationOptions
        , field "host_notification_commands" contactHostNotificationCommands
        , field "service_notification_commands" contactServiceNotificationCommands
        , field "email" contactEmail
        , field "can_submit_commands" contactCanSubmitCommands
        , field "retain_status_information" contactRetainStatusInformation
        , field "retain_nonstatus_information" contactRetainNonStatusInformation
        ]

instance Serializable ContactGroup where
    serialize ContactGroup{..} = catMaybes
        [ field "contactgroup_name" contactGroupName
        , field "alias" contactGroupAlias
        , lfield "members" contactGroupMembers
        ]

instance Encodable v => Serializable (Weekday v) where
    serialize (Monday v) = catMaybes [field "monday" v]
    serialize (Tuesday v) = catMaybes [field "tuesday" v]
    serialize (Wednesday v) = catMaybes [field "wednesday" v]
    serialize (Thursday v) = catMaybes [field "thursday" v]
    serialize (Friday v) = catMaybes [field "friday" v]
    serialize (Saterday v) = catMaybes [field "saturday" v]
    serialize (Sunday v) = catMaybes [field "sunday" v]

class Encodable x where
    encode :: x -> Maybe String

    encodeList :: [x] -> Maybe String
    encodeList = encode . intercalate "," . mapMaybe encode

instance Encodable Bool where encode flag = encode (if flag then "1" else "0")
instance Encodable Command where encode = encode . commandName
instance Encodable Contact where encode = encode . contactName
instance Encodable ContactGroup where encode = encode . contactGroupName
instance Encodable Host where encode = encode . hostName
instance Encodable String where encode = Just
instance Encodable TimePeriod where encode = encode . timePeriodName

instance Encodable HostNotificationOption where
    encode HostNotificationDown              = Just "d"
    encode HostNotificationUnreachable       = Just "u"
    encode HostNotificationRecovery          = Just "r"
    encode HostNotificationFlapping          = Just "f"
    encode HostNotificationScheduledDowntime = Just "s"

instance Encodable ServiceNotificationOption where
    encode ServiceNotificationWarning           = Just "w"
    encode ServiceNotificationUnknown           = Just "u"
    encode ServiceNotificationCritical          = Just "c"
    encode ServiceNotificationRecovery          = Just "r"
    encode ServiceNotificationFlapping          = Just "f"
    encode ServiceNotificationScheduledDowntime = Just "s"

instance Encodable a => Encodable (Maybe a) where
    encode Nothing = Nothing
    encode (Just x) = encode x
