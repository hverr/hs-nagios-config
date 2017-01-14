module Nagios.Config.EDSL.Types where

import Data.String (IsString(..))

-- | A host definition is used to define a physical server, workstation,
-- device, etc. that resides on your network.
data Host = Host { hostUse :: Maybe (Use Host)
                 , hostName :: Maybe HostName
                 , hostAlias :: Maybe Alias
                 , hostDisplayName :: Maybe DisplayName
                 , hostAddress :: Maybe Address
                 , hostParents :: Maybe (Parents Host)
                 , hostGroups :: Maybe HostGroups
                 , hostCheckCommand :: Maybe CheckCommand
                 , hostMaxCheckAttempts :: Maybe MaxCheckAttemts
                 , hostNotes :: Maybe Notes , hostCheckPeriod :: Maybe CheckPeriod , hostContacts :: Maybe Contacts
                 , hostContactGroups :: Maybe ContactGroups
                 , hostNotificationInterval :: Maybe NotificationInterval
                 , hostNotificationPeriod :: Maybe NotificationPeriod
                 }

-- | A host gruop definition is used to gruop one or more hosts together for
-- simplifying configuration, or display purposes.
data HostGroup = HostGroup { hostGroupName :: HostGroupName
                           , hostGroupAlias :: Alias
                           , hostGroupMembers :: Maybe (Members Host)
                           , hostGroupHostGroupMembers :: Maybe HostGroupMembers
                           , hostGroupNotes :: Maybe Notes
                           }

data Service = Service { serviceUse :: Maybe (Use Service)
                       , serviceHostGroupName :: Maybe HostGroupName
                       , serviceDescription :: ServiceDescription
                       , serviceCheckCommand :: CheckCommand
                       , serviceMaxCheckAttempts :: Maybe MaxCheckAttemts
                       , serviceCheckInterval :: Maybe CheckInterval
                       , serviceRetryInterval :: Maybe RetryInterval
                       , serviceCheckPeriod :: Maybe CheckPeriod
                       , serviceNotificationInterval :: Maybe NotificationInterval
                       , serviceNotificationPeriod :: Maybe NotificationPeriod
                       , serviceContacts :: Maybe Contacts
                       , serviceContactGroups :: Maybe ContactGroups
                       , serviceNotes :: Maybe Notes
                       }

data ServiceGroup = ServiceGroup { serviceGroupName :: ServiceGroupName
                                 , serviceGroupAlias :: Alias
                                 , serviceGroupMembers :: Maybe (Members Service)
                                 , serviceGroupNotes :: Maybe Notes
                                 }

data Command = Command { commandName :: CommandName
                       , commandLine :: CommandLine
                       }

data TimePeriod = TimePeriod { timePeriodName :: TimePeriodName
                             , timePeriodAlias :: Alias
                             , timePeriodWeekdays :: [Weekday String]
                             }

data Contact = Contact { contactUse :: Maybe (Use Contact)
                       , contactName :: ContactName
                       , contactAlias :: Maybe Alias
                       , contactGroups :: ContactGroups
                       , contactHostNotificationsEnabled :: Maybe HostNotificationsEnabled
                       , contactServiceNotificationsEnabled :: Maybe ServiceNotificationsEnabled
                       , contactHostNotificationPeriod :: Maybe HostNotificationPeriod
                       , contactServiceNotificationPeriod :: Maybe ServiceNotificationPeriod
                       , contactHostNotificationOptions :: Maybe HostNotificationsOptions
                       , contactServiceNotificationOptions :: Maybe ServiceNotificationOptions
                       , contactHostNotificationCommands :: Maybe HostNotificationCommands
                       , contactServiceNotificationCommands :: Maybe ServiceNotificationCommands
                       , contactEmail :: Maybe Email
                       , contactCanSubmitCommands :: Maybe CanSubmitCommands
                       , contactRetainStatusInformation :: Maybe RetainStatusInformation
                       , contactRetainNonStatusInformation :: Maybe RetainNonStatusInformation
                       }

data ContactGroup = ContactGroup { contactGroupName :: ContactGroupName
                                 , contactGroupAlias :: Alias
                                 , contactGroupMembers :: Maybe (Members Contact)
                                 }

newtype Address = Address String
newtype Alias = Alias String
newtype CanSubmitCommands = CanSubmitCommands Bool
newtype CheckCommand = CheckCommand Command
newtype CheckInterval = CheckInterval Int
newtype CheckPeriod = CheckPeriod TimePeriod
newtype CommandLine = CommandLine String
newtype CommandName = CommandName String
newtype ContactGroupName = ContactGroupName String
newtype ContactGroups = ContactGroups [ContactGroups]
newtype ContactName = ContactName String
newtype Contacts = Contacts [Contact]
newtype DisplayName = DisplayName String
newtype Email = Email String
newtype HostGroupMembers = HostGroupMembers [HostGroup]
newtype HostGroupName = HostGroupName String
newtype HostGroups = HostGroups [HostGroup]
newtype HostName = HostName String
newtype HostNotificationCommands = HostNotificationCommands Command
newtype HostNotificationPeriod = HostNotificationPeriod TimePeriod
newtype HostNotificationsEnabled = HostNotificationsEnabled Bool
newtype HostNotificationsOptions = HostNotificationsOptions [HostNotificationOption]
newtype MaxCheckAttemts = MaxCheckAttemts Int
newtype Members a = Members [a]
newtype Notes = Notes String
newtype NotificationInterval = NotificationInterval Int
newtype NotificationPeriod = NotificationPeriod Int
newtype Parents a = Parents [a]
newtype RetainNonStatusInformation = RetainNonStatusInformation Bool
newtype RetainStatusInformation = RetainStatusInformation Bool
newtype RetryInterval = RetryInterval Int
newtype ServiceDescription = ServiceDescription String
newtype ServiceGroupName = ServiceGroupName String
newtype ServiceNotificationCommands = ServiceNotificationCommands Command
newtype ServiceNotificationOptions = ServiceNotificationOptions [ServiceNotificationOption]
newtype ServiceNotificationPeriod = ServiceNotificationPeriod TimePeriod
newtype ServiceNotificationsEnabled = ServiceNotificationsEnabled Bool
newtype TimePeriodName = TimePeriodName String
newtype Use a = Use a

instance IsString Address            where fromString = Address
instance IsString Alias              where fromString = Alias
instance IsString CommandLine        where fromString = CommandLine
instance IsString CommandName        where fromString = CommandName
instance IsString ContactGroupName   where fromString = ContactGroupName
instance IsString ContactName        where fromString = ContactName
instance IsString DisplayName        where fromString = DisplayName
instance IsString Email              where fromString = Email
instance IsString HostGroupName      where fromString = HostGroupName
instance IsString HostName           where fromString = HostName
instance IsString Notes              where fromString = Notes
instance IsString ServiceDescription where fromString = ServiceDescription
instance IsString ServiceGroupName   where fromString = ServiceGroupName
instance IsString TimePeriodName     where fromString = TimePeriodName

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
