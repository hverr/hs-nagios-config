module Nagios.Config.EDSL.Types where

-- | A host definition is used to define a physical server, workstation,
-- device, etc. that resides on your network.
data Host = Host { hostUse :: Maybe Host
                 , hostName :: Maybe String
                 , hostAlias :: Maybe String
                 , hostDisplayName :: Maybe String
                 , hostAddress :: Maybe String
                 , hostParents :: [Host]
                 , hostGroups :: [HostGroup]
                 , hostCheckCommand :: Maybe Command
                 , hostMaxCheckAttempts :: Maybe Int
                 , hostNotes :: Maybe String
                 , hostCheckPeriod :: Maybe TimePeriod
                 , hostContactGroups :: [ContactGroup]
                 , hostNotificationInterval :: Maybe TimePeriod
                 , hostNotificationPeriod :: Maybe TimePeriod
                 }

-- | A host gruop definition is used to gruop one or more hosts together for
-- simplifying configuration, or display purposes.
data HostGroup = HostGroup { hostGroupName :: String
                           , hostGroupAlias :: String
                           , hostGroupMembers :: [Host]
                           , hostGroupHostGroupMembers :: [HostGroup]
                           , hostGroupNotes :: Maybe String
                           }

data Service = Service { serviceUse :: Maybe Service
                       , serviceHostGroupName :: Maybe String
                       , serviceDescription :: String
                       , serviceCheckCommand :: Command
                       , serviceMaxCheckAttempts :: Maybe Int
                       , serviceCheckInterval :: Maybe Int
                       , serviceRetryInterval :: Maybe Int
                       , serviceCheckPeriod :: Maybe TimePeriod
                       , serviceNotificationInterval :: Maybe Int
                       , serviceNotificationPeriod :: Maybe TimePeriod
                       , serviceContacts :: [Contact]
                       , serviceContactGroups :: [ContactGroup]
                       , serviceNotes :: Maybe String
                       }

data ServiceGroup = ServiceGroup { serviceGroupName :: String
                                 , serviceGroupAlias :: String
                                 , serviceGroupMembers :: [Service]
                                 , serviceGroupNotes :: Maybe String
                                 }

data Command = Command { commandName :: String
                       , commandLine :: String
                       }

data TimePeriod = TimePeriod { timePeriodName :: String
                             , timePeriodAlias :: String
                             , timePeriodWeekdays :: [Weekday String]
                             }

data Contact = Contact { contactUse :: Maybe Contact
                       , contactName :: String
                       , contactAlias :: Maybe String
                       , contactGroups :: [ContactGroup]
                       , contactHostNotificationsEnabled :: Maybe Bool
                       , contactServiceNotificationsEnabled :: Maybe Bool
                       , contactHostNotificationPeriod :: Maybe TimePeriod
                       , contactServiceNotificationPeriod :: Maybe TimePeriod
                       , contactHostNotificationOptions :: [HostNotificationOption]
                       , contactServiceNotificationOptions :: [ServiceNotificationOption]
                       , contactHostNotificationCommands :: Maybe Command
                       , contactServiceNotificationCommands :: Maybe Command
                       , contactEmail :: Maybe String
                       , contactCanSubmitCommands :: Maybe Bool
                       , contactRetainStatusInformation :: Maybe Bool
                       , contactRetainNonStatusInformation :: Maybe Bool
                       }

data ContactGroup = ContactGroup { contactGroupName :: String
                                 , contactGroupAlias :: String
                                 , contactGroupMembers :: [Contact]
                                 }

data ServiceNotificationOption = ServiceNotificationWarning
                               | ServiceNotificationUnknown
                               | ServiceNotificationCritical
                               | ServiceNotificationRecovery
                               | ServiceNotificationFlapping

data HostNotificationOption = HostNotificationDown
                            | HostNotificationUnreachable
                            | HostNotificationRecovery
                            | HostNotificationFlapping
                            | HostNotificationScheduledDowntime

data Weekday a = Monday a
               | Tuesday a
               | Wednesday a
               | Thursday a
               | Friday a
               | Saterday a
               | Sunday a
