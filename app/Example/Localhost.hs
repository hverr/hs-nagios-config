module Example.Localhost where


import Nagios.Config.EDSL
import Nagios.Config.EDSL.Defaults (linuxServer)

localhost :: Host
localhost = (host "localhost")
    { hostUse = Just linuxServer
    , hostHostName = Just "localhost"
    , hostAlias = Just "localhost"
    , hostAddress = Just "127.0.0.1"
    }
