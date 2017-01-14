module Nagios.Config.EDSL.Defaults.Hosts where

import Nagios.Config.EDSL.Defaults.Commands (checkHostAlive)
import Nagios.Config.EDSL.Defaults.Contacts (admins)
import Nagios.Config.EDSL.Defaults.TimePeriods (always)
import Nagios.Config.EDSL.Types

genericHost :: Host
genericHost = (host "generic-host")
    { hostNotificationsEnabled = Just True
    , hostEventHandlerEnabled = Just True
    , hostFlapDetectionEnabled = Just True
    , hostProcessPerfData = Just True
    , hostRetainStatusInformation = Just True
    , hostRetainNonStatusInformation = Just True
    , hostNotificationPeriod = Just always
    , hostRegister = Just False
    }

linuxServer :: Host
linuxServer = (host "linux-server")
    { hostUse = Just genericHost
    , hostCheckPeriod = Just always
    , hostCheckInterval = Just 5
    , hostRetryInterval = Just 1
    , hostMaxCheckAttempts = Just 10
    , hostCheckCommand = Just checkHostAlive
    , hostNotificationPeriod = Just always
    , hostNotificationInterval = Just 120
    , hostNotificationOptions = [HostNotificationDown,
                                 HostNotificationUnreachable,
                                 HostNotificationRecovery]
    , hostContactGroups = [admins]
    , hostRegister = Just False
    }

