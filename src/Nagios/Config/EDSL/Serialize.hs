{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Nagios.Config.EDSL.Serialize where

import Data.Maybe (catMaybes, mapMaybe)
import Data.List (intercalate)

import Nagios.Config.EDSL.Types

writeConfiguration :: [Object] -> String
writeConfiguration = concatMap writeObject . resolve

writeObject :: ObjectType x => x -> String
writeObject x = "define " ++ objectType x ++ "{\n" ++ fields' ++ "\t}\n"
  where
    fields' = concatMap writeField (serialize x)
    writeField (Field key value) = "\t" ++ key ++ " " ++ value ++ "\n"

resolve :: [Object] -> [Object]
resolve = resolve' []
  where
    resolve' l [] = l
    resolve' l (x:xs)
        | x `elem'` l = resolve' l xs
        | otherwise = resolve' (x:l) (xs ++ dependencies x)

    elem' :: ObjectType a => a -> [a] -> Bool
    elem' _ [] = False
    elem' y (x:xs) | objectSame x y = True
                   | otherwise = elem' y xs

data Field = Field String String deriving (Show)

field :: Encodable v => String -> v -> Maybe Field
field name value = Field name <$> encode value

lfield :: Encodable v => String -> [v] -> Maybe Field
lfield name value = Field name <$> encodeList value

class Serializable x => ObjectType x where
    objectType :: x -> String
    objectSame :: x -> x -> Bool

class Serializable x where
    serialize :: x -> [Field]
    dependencies :: x -> [Object]

data Object = OHost Host
            | OHostGroup HostGroup
            | OService Service
            | OServiceGroup ServiceGroup
            | OContact Contact
            | OContactGroup ContactGroup
            | OTimePeriod TimePeriod
            | OCommand Command
            deriving (Show)

instance ObjectType Object where
    objectType (OHost x) = objectType x
    objectType (OHostGroup x) = objectType x
    objectType (OService x) = objectType x
    objectType (OServiceGroup x) = objectType x
    objectType (OContact x) = objectType x
    objectType (OContactGroup x) = objectType x
    objectType (OTimePeriod x) = objectType x
    objectType (OCommand x) = objectType x

    objectSame (OHost a) (OHost b) = objectSame a b
    objectSame (OHostGroup a) (OHostGroup b) = objectSame a b
    objectSame (OService a) (OService b) = objectSame a b
    objectSame (OServiceGroup a) (OServiceGroup b) = objectSame a b
    objectSame (OContact a) (OContact b) = objectSame a b
    objectSame (OContactGroup a) (OContactGroup b) = objectSame a b
    objectSame (OTimePeriod a) (OTimePeriod b) = objectSame a b
    objectSame (OCommand a) (OCommand b) = objectSame a b
    objectSame _ _ = False

instance Serializable Object where
    serialize (OHost x) = serialize x
    serialize (OHostGroup x) = serialize x
    serialize (OService x) = serialize x
    serialize (OServiceGroup x) = serialize x
    serialize (OContact x) = serialize x
    serialize (OContactGroup x) = serialize x
    serialize (OTimePeriod x) = serialize x
    serialize (OCommand x) = serialize x

    dependencies (OHost x) = dependencies x
    dependencies (OHostGroup x) = dependencies x
    dependencies (OService x) = dependencies x
    dependencies (OServiceGroup x) = dependencies x
    dependencies (OContact x) = dependencies x
    dependencies (OContactGroup x) = dependencies x
    dependencies (OTimePeriod x) = dependencies x
    dependencies (OCommand x) = dependencies x

instance ObjectType Host where
    objectType _ = "host"
    objectSame a b = hostName a == hostName b

instance Serializable Host where
    dependencies Host{..} = catMaybes $
        [ OHost <$> hostUse ] ++
        map (Just . OHost) hostParents ++
        map (Just . OHostGroup) hostGroups ++
        [ OCommand . command <$> hostCheckCommand
        , OTimePeriod <$> hostCheckPeriod ] ++
        map (Just . OContactGroup) hostContactGroups ++
        [OTimePeriod <$> hostNotificationPeriod]

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
    objectSame a b = hostGroupName a == hostGroupName b

instance Serializable HostGroup where
    dependencies HostGroup{..} =
        map OHost hostGroupMembers ++
        map OHostGroup hostGroupHostGroupMembers

    serialize HostGroup{..} = catMaybes
        [ field "hostgroup_name" hostGroupName
        , field "alias" hostGroupAlias
        , lfield "members" hostGroupMembers
        , lfield "hostgroup_members" hostGroupHostGroupMembers
        , field "notes" hostGroupNotes
        ]

instance ObjectType Service where
    objectType _ = "service"
    objectSame a b = serviceName a == serviceName b

instance Serializable Service where
    dependencies Service{..} = catMaybes $
        [ OService <$> serviceUse ] ++
        map (Just . OHost) serviceHosts ++
        map (Just . OHostGroup) serviceHostGroups ++
        [ OCommand . command <$> serviceCheckCommand
        , OTimePeriod <$> serviceCheckPeriod
        , OCommand . command <$> serviceEventHandler
        , OTimePeriod <$> serviceNotificationPeriod
        ] ++
        map (Just . OContact) serviceContacts ++
        map (Just . OContactGroup) serviceContactGroups

    serialize Service{..} = catMaybes
        [ field "use" serviceUse
        , field "name" serviceName
        , lfield "host_name" serviceHosts
        , lfield "hostgroup_name" serviceHostGroups
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
    objectSame a b = serviceGroupName a == serviceGroupName b

instance Serializable ServiceGroup where
    dependencies ServiceGroup{..} =
        map OService serviceGroupMembers

    serialize ServiceGroup{..} = catMaybes
        [ field "servicegroup_name" serviceGroupName
        , field "alias" serviceGroupAlias
        , lfield "members" serviceGroupMembers
        , field "notes" serviceGroupNotes
        ]

instance ObjectType Command where
    objectType _ = "command"
    objectSame a b = commandName a == commandName b

instance Serializable Command where
    dependencies _ = []
    serialize Command{..} = catMaybes
        [ field "command_name" commandName
        , field "command_line" commandLine
        ]

instance ObjectType TimePeriod where
    objectType _ = "timeperiod"
    objectSame a b = timePeriodName a == timePeriodName b

instance Serializable TimePeriod where
    dependencies _ = []
    serialize TimePeriod{..} = catMaybes
        [ field "timeperiod_name" timePeriodName
        , field "alias" timePeriodAlias ] ++
        concatMap serialize timePeriodWeekdays

instance ObjectType Contact where
    objectType _ = "contact"
    objectSame a b = contactName a == contactName b

instance Serializable Contact where
    dependencies Contact{..} = catMaybes $
        [ OContact <$> contactUse ] ++
        map (Just . OContactGroup) contactGroups ++
        [ OTimePeriod <$> contactHostNotificationPeriod
        , OTimePeriod <$> contactServiceNotificationPeriod
        , OCommand . command <$> contactHostNotificationCommands
        , OCommand . command<$> contactServiceNotificationCommands
        ]

    serialize Contact{..} = catMaybes
        [ field "use" contactUse
        , field "name" contactName
        , field "contact_name" contactName
        , field "alias" contactAlias
        , lfield "contactgroups" contactGroups
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
    objectSame a b = contactGroupName a == contactGroupName b

instance Serializable ContactGroup where
    dependencies ContactGroup{..} =
        map OContact contactGroupMembers

    serialize ContactGroup{..} = catMaybes
        [ field "contactgroup_name" contactGroupName
        , field "alias" contactGroupAlias
        , lfield "members" contactGroupMembers
        ]

instance Encodable v => Serializable (Weekday v) where
    dependencies _ = []
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
    encodeList xs = case mapMaybe encode xs of
                        [] -> Nothing
                        xs' -> encode $ intercalate "," xs'

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

instance Encodable CommandApp where
    encode (CommandApp cmd args) = encode $ commandName cmd ++ "!" ++ intercalate "!" args

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
