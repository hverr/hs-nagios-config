module Nagios.Config.EDSL.Types where

-- | A host definition is used to define a physical server, workstation,
-- device, etc. that resides on your network.
data Host = Host { hostUse :: Maybe Host
                 , hostName :: String
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

-- | Create a new host with a specific name
host :: String -> Host
host name = Host { hostUse = Nothing
                 , hostName = name
                 , hostAlias = Nothing
                 , hostDisplayName = Nothing
                 , hostAddress = Nothing
                 , hostParents = []
                 , hostGroups = []
                 , hostCheckCommand = Nothing
                 , hostMaxCheckAttempts = Nothing
                 , hostNotes = Nothing
                 , hostCheckPeriod = Nothing
                 , hostContactGroups = []
                 , hostNotificationInterval = Nothing
                 , hostNotificationPeriod = Nothing
                 }

-- | A host gruop definition is used to gruop one or more hosts together for
-- simplifying configuration, or display purposes.
data HostGroup = HostGroup { hostGroupName :: String
                           , hostGroupAlias :: String
                           , hostGroupMembers :: [Host]
                           , hostGroupHostGroupMembers :: [HostGroup]
                           , hostGroupNotes :: Maybe String
                           }

-- | A service definition is used to identify a "service" that runs on a host.
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

-- | A service group definition is used to group on ore more services together.
data ServiceGroup = ServiceGroup { serviceGroupName :: String
                                 , serviceGroupAlias :: String
                                 , serviceGroupMembers :: [Service]
                                 , serviceGroupNotes :: Maybe String
                                 }

-- | A command definition defines a command.
data Command = Command { commandName :: String
                       , commandLine :: String
                       }

-- | A time period is a list of times during various days.
data TimePeriod = TimePeriod { timePeriodName :: String
                             , timePeriodAlias :: String
                             , timePeriodWeekdays :: [Weekday String]
                             }

-- | A contact definition is used to identify someone who should be contacted
-- in the event of a problem on your network.
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

-- | Create a new contact with the specified name
contact :: String -> Contact
contact name = Contact { contactUse = Nothing
                       , contactName = name
                       , contactAlias = Nothing
                       , contactGroups = []
                       , contactHostNotificationsEnabled = Nothing
                       , contactServiceNotificationsEnabled = Nothing
                       , contactHostNotificationPeriod = Nothing
                       , contactServiceNotificationPeriod = Nothing
                       , contactHostNotificationOptions = []
                       , contactServiceNotificationOptions = []
                       , contactHostNotificationCommands = Nothing
                       , contactServiceNotificationCommands = Nothing
                       , contactEmail = Nothing
                       , contactCanSubmitCommands = Nothing
                       , contactRetainStatusInformation = Nothing
                       , contactRetainNonStatusInformation = Nothing
                       }

-- | A contact group definition is used to group one ore more contacts together.
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
