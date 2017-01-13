module Nagios.Config.EDSL.Types where

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
                 , hostNotes :: Maybe Notes
                 , hostCheckPeriod :: Maybe CheckPeriod
                 , hostContacts :: Maybe Contacts
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
                             }

data Contact = Contact { contactName :: ContactName }

data ContactGroup = ContactGroup { contactGroupName :: ContactGroupName
                                 , contactGroupAlias :: Alias
                                 , contactGroupMembers :: Maybe (Members Contact)
                                 }

newtype Address = Address String
newtype Alias = Alias String
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
newtype HostGroupMembers = HostGroupMembers [HostGroup]
newtype HostGroupName = HostGroupName String
newtype HostGroups = HostGroups [HostGroup]
newtype HostName = HostName String
newtype MaxCheckAttemts = MaxCheckAttemts Int
newtype Members a = Members [a]
newtype Notes = Notes String
newtype NotificationInterval = NotificationInterval Int
newtype NotificationPeriod = NotificationPeriod Int
newtype Parents a = Parents [a]
newtype RetryInterval = RetryInterval Int
newtype ServiceDescription = ServiceDescription String
newtype ServiceGroupName = ServiceGroupName String
newtype TimePeriodName = TimePeriodName String
newtype Use a = Use a
