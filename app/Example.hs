module Main where

import Nagios.Config.EDSL
import Nagios.Config.EDSL.Defaults hiding (localhost)

main :: IO ()
main = putStr $ writeConfiguration toplevel

toplevel :: [Object]
toplevel =
    [ OService ping
    ]

-- ------------------------------
-- --------- HOSTGROUPS ---------
-- ------------------------------
allHosts :: HostGroup
allHosts = (hostgroup "all-servers" "All Servers")
    { hostGroupMembers = [localhost] }

-- ---------------------------
-- --------- SERVICES --------
-- ---------------------------
ping :: Service
ping = (service "ping")
    { serviceUse = Just localService
    , serviceDescription = Just "PING"
    , serviceHostGroups = [allHosts]
    , serviceCheckCommand = Just $ apply checkPing ["100.0,20%", "500.0,60%"]
    }

-- --------------------------
-- --------- HOSTS ----------
-- --------------------------
localhost :: Host
localhost = (host "localhost")
    { hostUse = Just linuxServer
    , hostHostName = Just "localhost"
    , hostAlias = Just "localhost"
    , hostAddress = Just "127.0.0.1"
    }
