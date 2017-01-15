module Nagios.Config.EDSL.Types where

-- | A host definition is used to define a physical server, workstation,
-- device, etc. that resides on your network.
data Host = Host { hostUse :: Maybe Host
                 , hostName :: String
                 , hostHostName :: Maybe String
                 , hostAlias :: Maybe String
                 , hostDisplayName :: Maybe String
                 , hostAddress :: Maybe String
                 , hostParents :: [Host]
                 , hostGroups :: [HostGroup]
                 , hostCheckCommand :: Maybe Command
                 , hostMaxCheckAttempts :: Maybe Int
                 , hostCheckInterval :: Maybe Int
                 , hostRetryInterval :: Maybe Int
                 , hostNotes :: Maybe String
                 , hostCheckPeriod :: Maybe TimePeriod
                 , hostEventHandlerEnabled :: Maybe Bool
                 , hostFlapDetectionEnabled :: Maybe Bool
                 , hostProcessPerfData :: Maybe Bool
                 , hostRetainStatusInformation :: Maybe Bool
                 , hostRetainNonStatusInformation :: Maybe Bool
                 , hostContactGroups :: [ContactGroup]
                 , hostNotificationInterval :: Maybe Int
                 , hostNotificationPeriod :: Maybe TimePeriod
                 , hostNotificationOptions :: [HostNotificationOption]
                 , hostNotificationsEnabled :: Maybe Bool
                 , hostRegister :: Maybe Bool
                 }

-- | Create a new host with a specific name
host :: String -> Host
host name = Host { hostUse = Nothing
                 , hostHostName = Nothing
                 , hostName = name
                 , hostAlias = Nothing
                 , hostDisplayName = Nothing
                 , hostAddress = Nothing
                 , hostParents = []
                 , hostGroups = []
                 , hostCheckCommand = Nothing
                 , hostMaxCheckAttempts = Nothing
                 , hostCheckInterval = Nothing
                 , hostRetryInterval = Nothing
                 , hostNotes = Nothing
                 , hostCheckPeriod = Nothing
                 , hostEventHandlerEnabled = Nothing
                 , hostFlapDetectionEnabled = Nothing
                 , hostProcessPerfData = Nothing
                 , hostRetainStatusInformation = Nothing
                 , hostRetainNonStatusInformation = Nothing
                 , hostContactGroups = []
                 , hostNotificationInterval = Nothing
                 , hostNotificationPeriod = Nothing
                 , hostNotificationOptions = []
                 , hostNotificationsEnabled = Nothing
                 , hostRegister = Nothing
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
                       , serviceName :: String
                       , serviceHostGroupName :: Maybe String
                       , serviceDescription :: Maybe String
                       , serviceDisplayName :: Maybe String
                       , serviceIsVolatile :: Maybe Bool
                       , serviceCheckCommand :: Maybe Command
                       , serviceInitialState :: Maybe ServiceState
                       , serviceMaxCheckAttempts :: Maybe Int
                       , serviceCheckInterval :: Maybe Int
                       , serviceRetryInterval :: Maybe Int
                       , serviceActiveChecksEnabled :: Maybe Bool
                       , servicePassiveChecksEnabled :: Maybe Bool
                       , serviceParallelizeCheck :: Maybe Bool
                       , serviceCheckPeriod :: Maybe TimePeriod
                       , serviceObsessOverService :: Maybe Bool
                       , serviceCheckFreshness :: Maybe Bool
                       , serviceFreshnessThreshold :: Maybe Int
                       , serviceEventHandler :: Maybe Command
                       , serviceEventHandlerEnabled :: Maybe Bool
                       , serviceFlapDetectionEnabled :: Maybe Bool
                       , serviceProcessPerfData :: Maybe Bool
                       , serviceRetainStatusInformation :: Maybe Bool
                       , serviceRetainNonStatusInformation :: Maybe Bool
                       , serviceNotificationInterval :: Maybe Int
                       , serviceNotificationPeriod :: Maybe TimePeriod
                       , serviceNotificationOptions :: [ServiceNotificationOption]
                       , serviceNotificationsEnabled :: Maybe Bool
                       , serviceContacts :: [Contact]
                       , serviceContactGroups :: [ContactGroup]
                       , serviceNotes :: Maybe String
                       , serviceRegister :: Maybe Bool
                       }

-- | Create a new service with a specified name.
service :: String -> Service
service name = Service { serviceUse = Nothing
                       , serviceName = name
                       , serviceHostGroupName = Nothing
                       , serviceDescription = Nothing
                       , serviceDisplayName = Nothing
                       , serviceIsVolatile = Nothing
                       , serviceCheckCommand = Nothing
                       , serviceInitialState = Nothing
                       , serviceMaxCheckAttempts = Nothing
                       , serviceCheckInterval = Nothing
                       , serviceRetryInterval = Nothing
                       , serviceActiveChecksEnabled = Nothing
                       , servicePassiveChecksEnabled = Nothing
                       , serviceParallelizeCheck = Nothing
                       , serviceCheckPeriod = Nothing
                       , serviceObsessOverService = Nothing
                       , serviceCheckFreshness = Nothing
                       , serviceFreshnessThreshold = Nothing
                       , serviceEventHandler = Nothing
                       , serviceEventHandlerEnabled = Nothing
                       , serviceFlapDetectionEnabled = Nothing
                       , serviceProcessPerfData = Nothing
                       , serviceRetainStatusInformation = Nothing
                       , serviceRetainNonStatusInformation = Nothing
                       , serviceNotificationInterval = Nothing
                       , serviceNotificationPeriod = Nothing
                       , serviceNotificationOptions = []
                       , serviceNotificationsEnabled = Nothing
                       , serviceContacts = []
                       , serviceContactGroups = []
                       , serviceNotes = Nothing
                       , serviceRegister = Nothing
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
                       , contactRegister :: Maybe Bool
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
                       , contactRegister = Nothing
                       }

-- | A contact group definition is used to group one ore more contacts together.
data ContactGroup = ContactGroup { contactGroupName :: String
                                 , contactGroupAlias :: String
                                 , contactGroupMembers :: [Contact]
                                 }

-- | Create a new contact group with the specified name and alias
contactgroup :: String -> String -> ContactGroup
contactgroup name alias = ContactGroup { contactGroupName = name
                                       , contactGroupAlias = alias
                                       , contactGroupMembers = []
                                       }

data ServiceState = ServiceStateOK
                  | ServiceStateWarning
                  | ServiceStateUnknown
                  | ServiceStateCritical

data ServiceNotificationOption = ServiceNotificationWarning
                               | ServiceNotificationUnknown
                               | ServiceNotificationCritical
                               | ServiceNotificationRecovery
                               | ServiceNotificationFlapping
                               | ServiceNotificationScheduledDowntime

serviceNotificationAlways :: [ServiceNotificationOption]
serviceNotificationAlways = [ServiceNotificationWarning
                            , ServiceNotificationUnknown
                            , ServiceNotificationCritical
                            , ServiceNotificationRecovery
                            , ServiceNotificationFlapping
                            , ServiceNotificationScheduledDowntime]


data HostNotificationOption = HostNotificationDown
                            | HostNotificationUnreachable
                            | HostNotificationRecovery
                            | HostNotificationFlapping
                            | HostNotificationScheduledDowntime

hostNotificationAlways :: [HostNotificationOption]
hostNotificationAlways = [HostNotificationDown
                         , HostNotificationUnreachable
                         , HostNotificationRecovery
                         , HostNotificationFlapping
                         , HostNotificationScheduledDowntime]


data Weekday a = Monday a
               | Tuesday a
               | Wednesday a
               | Thursday a
               | Friday a
               | Saterday a
               | Sunday a
