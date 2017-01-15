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

class Serializable x => ObjectType x where
    objectType :: x -> String

class Serializable x where
    serialize :: x -> [Field]

data Object = OHost Host
            | OHostGroup HostGroup
            | OService Service
            | OServiceGroup ServiceGroup
            | OContact Contact
            | OContactGroup ContactGroup
            | OTimePeriod TimePeriod
            | OCommand Command

instance ObjectType Object where
    objectType (OHost x) = objectType x
    objectType (OHostGroup x) = objectType x
    objectType (OService x) = objectType x
    objectType (OServiceGroup x) = objectType x
    objectType (OContact x) = objectType x
    objectType (OContactGroup x) = objectType x
    objectType (OTimePeriod x) = objectType x
    objectType (OCommand x) = objectType x

instance Serializable Object where
    serialize (OHost x) = serialize x
    serialize (OHostGroup x) = serialize x
    serialize (OService x) = serialize x
    serialize (OServiceGroup x) = serialize x
    serialize (OContact x) = serialize x
    serialize (OContactGroup x) = serialize x
    serialize (OTimePeriod x) = serialize x
    serialize (OCommand x) = serialize x

instance ObjectType Host where
    objectType _ = "host"

instance Serializable Host where
    serialize Host{..} = catMaybes
        [ field "use" hostUse
        , field "name" hostName
        , field "host_name" hostHostName
        , field "alias" hostAlias
        , field "display_name" hostDisplayName
        , field "address" hostAddress
        , lfield "parents" hostParents
        , lfield "hostgroups" hostGroups
        , field "check_command" hostCheckCommand
        , field "max_check_attempts" hostMaxCheckAttempts
        , field "check_interval" hostCheckInterval
        , field "retry_interval" hostRetryInterval
        , field "notes" hostNotes
        , field "check_period" hostCheckPeriod
        , field "event_handler_enabled" hostEventHandlerEnabled
        , field "flap_detection_enabled" hostFlapDetectionEnabled
        , field "process_perf_data" hostProcessPerfData
        , field "retain_status_information" hostRetainStatusInformation
        , field "retain_nonstatus_information" hostRetainNonStatusInformation
        , lfield "contact_groups" hostContactGroups
        , field "notification_interval" hostNotificationInterval
        , field "notification_period" hostNotificationPeriod
        , lfield "notification_options" hostNotificationOptions
        , field "notifications_enabled" hostNotificationsEnabled
        , field "register" hostRegister
        ]

instance ObjectType HostGroup where
    objectType _ = "hostgroup"

instance Serializable HostGroup where
    serialize HostGroup{..} = catMaybes
        [ field "hostgroup_name" hostGroupName
        , field "alias" hostGroupAlias
        , lfield "members" hostGroupMembers
        , lfield "hostgroup_members" hostGroupHostGroupMembers
        , field "notes" hostGroupNotes
        ]

instance ObjectType Service where
    objectType _ = "service"

instance Serializable Service where
    serialize Service{..} = catMaybes
        [ field "use" serviceUse
        , field "name" serviceName
        , field "hostgroup_name" serviceHostGroupName
        , field "service_description" serviceDescription
        , field "display_name" serviceDisplayName
        , field "is_volatile" serviceIsVolatile
        , field "check_command" serviceCheckCommand
        , field "initial_state" serviceInitialState
        , field "max_check_attempts" serviceMaxCheckAttempts
        , field "normal_check_interval" serviceCheckInterval
        , field "retry_check_interval" serviceRetryInterval
        , field "active_checks_enabled" serviceActiveChecksEnabled
        , field "passive_checks_enabled" servicePassiveChecksEnabled
        , field "parallelize_check" servicePassiveChecksEnabled
        , field "check_period" serviceCheckPeriod
        , field "obsess_over_service" serviceObsessOverService
        , field "check_freshness" serviceCheckFreshness
        , field "event_handler" serviceEventHandler
        , field "event_handler_enabled" serviceEventHandlerEnabled
        , field "flap_detection_enabled" serviceFlapDetectionEnabled
        , field "process_perf_data" serviceProcessPerfData
        , field "retain_status_information" serviceRetainStatusInformation
        , field "retain_nonstatus_information" serviceRetainNonStatusInformation
        , field "notification_interval" serviceNotificationInterval
        , field "notification_period" serviceNotificationPeriod
        , lfield "notification_options" serviceNotificationOptions
        , field "notifications_enabled" serviceNotificationsEnabled
        , lfield "contacts" serviceContacts
        , lfield "contact_groups" serviceContactGroups
        , field "notes" serviceNotes
        , field "register" serviceRegister
        ]

instance ObjectType ServiceGroup where
    objectType _ = "servicegroup"

instance Serializable ServiceGroup where
    serialize ServiceGroup{..} = catMaybes
        [ field "servicegroup_name" serviceGroupName
        , field "alias" serviceGroupAlias
        , lfield "members" serviceGroupMembers
        , field "notes" serviceGroupNotes
        ]

instance ObjectType Command where
    objectType _ = "command"

instance Serializable Command where
    serialize Command{..} = catMaybes
        [ field "command_name" commandName
        , field "command_line" commandLine
        ]

instance ObjectType TimePeriod where
    objectType _ = "timeperiod"

instance Serializable TimePeriod where
    serialize TimePeriod{..} = catMaybes
        [ field "timeperiod_name" timePeriodName
        , field "alias" timePeriodAlias ] ++
        concatMap serialize timePeriodWeekdays

instance ObjectType Contact where
    objectType _ = "contact"

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
        , field "register" contactRegister
        ]

instance ObjectType ContactGroup where
    objectType _ = "contactgroup"

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
instance Encodable HostGroup where encode = encode . hostGroupName
instance Encodable Int where encode = encode . show
instance Encodable Service where encode = encode . serviceName
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

instance Encodable ServiceState where
    encode ServiceStateOK       = Just "o"
    encode ServiceStateWarning  = Just "w"
    encode ServiceStateUnknown  = Just "u"
    encode ServiceStateCritical = Just "c"

instance Encodable a => Encodable (Maybe a) where
    encode = (>>= encode)
