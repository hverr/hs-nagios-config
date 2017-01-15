{-# LANGUAGE OverloadedStrings #-}
module Nagios.Config.EDSL.Defaults.Contacts where

import Nagios.Config.EDSL.Defaults.TimePeriods (always)

import Nagios.Config.EDSL.Types

genericContact :: Contact
genericContact = (contact "generic-contact")
    { contactServiceNotificationPeriod = Just always
    , contactHostNotificationPeriod = Just always
    , contactServiceNotificationOptions = serviceNotificationAlways
    , contactHostNotificationOptions = hostNotificationAlways
    , contactServiceNotificationCommands = Just notifyServiceByEmail
    , contactHostNotificationCommands = Just notifyHostByEmail
    , contactRegister = Just False
    }

nagiosadmin :: Contact
nagiosadmin = (contact "nagiosadmin")
    { contactUse = Just genericContact
    , contactAlias = Just "Nagios Admin"
    , contactEmail = Just "nagios@localhost"
    }

admins :: ContactGroup
admins = (contactgroup "admins" "Nagios Administrators")
    { contactGroupMembers = [nagiosadmin] }

notifyHostByEmail :: CommandApp
notifyHostByEmail = flip apply [] $
                    Command "notify-host-by-email"
                            "/usr/bin/printf \"%b\" \"***** Nagios *****\\n\\nNotification Type: $NOTIFICATIONTYPE$\\nHost: $HOSTNAME$\\nState: $HOSTSTATE$\\nAddress: $HOSTADDRESS$\\nInfo: $HOSTOUTPUT$\\n\\nDate/Time: $LONGDATETIME$\\n\" | /bin/mail -s \"** $NOTIFICATIONTYPE$ Host Alert: $HOSTNAME$ is $HOSTSTATE$ **\" $CONTACTEMAIL$"

notifyServiceByEmail :: CommandApp
notifyServiceByEmail = flip apply [] $
                       Command "notify-service-by-email"
                               "/usr/bin/printf \"%b\" \"***** Nagios *****\\n\\nNotification Type: $NOTIFICATIONTYPE$\\n\\nService: $SERVICEDESC$\\nHost: $HOSTALIAS$\\nAddress: $HOSTADDRESS$\\nState: $SERVICESTATE$\\n\\nDate/Time: $LONGDATETIME$\\n\\nAdditional Info:\\n\\n$SERVICEOUTPUT$\\n\" | /bin/mail -s \"** $NOTIFICATIONTYPE$ Service Alert: $HOSTALIAS$/$SERVICEDESC$ is $SERVICESTATE$ **\" $CONTACTEMAIL$"
