module Nagios.Config.EDSL.Defaults.Services where

import Nagios.Config.EDSL.Defaults.Contacts (admins)
import Nagios.Config.EDSL.Defaults.TimePeriods (always)
import Nagios.Config.EDSL.Types

genericService :: Service
genericService = (service "generic-service")
    { serviceActiveChecksEnabled = Just True
    , servicePassiveChecksEnabled = Just True
    , serviceParallelizeCheck = Just True
    , serviceObsessOverService = Just True
    , serviceCheckFreshness = Just False
    , serviceNotificationsEnabled = Just True
    , serviceEventHandlerEnabled = Just True
    , serviceFlapDetectionEnabled = Just True
    , serviceProcessPerfData = Just True
    , serviceRetainStatusInformation = Just True
    , serviceRetainNonStatusInformation = Just True
    , serviceIsVolatile = Just False
    , serviceCheckPeriod = Just always
    , serviceMaxCheckAttempts = Just 3
    , serviceCheckInterval = Just 10
    , serviceRetryInterval = Just 2
    , serviceContactGroups = [admins]
    , serviceNotificationOptions = [ServiceNotificationWarning,
                                    ServiceNotificationUnknown,
                                    ServiceNotificationCritical,
                                    ServiceNotificationRecovery]
    , serviceNotificationInterval = Just 60
    , serviceNotificationPeriod = Just always
    , serviceRegister = Just False
    }
