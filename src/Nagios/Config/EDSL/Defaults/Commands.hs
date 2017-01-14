module Nagios.Config.EDSL.Defaults.Commands where

import Nagios.Config.EDSL.Types

checkHostAlive :: Command
checkHostAlive = Command "check-host-alive"
                         "$USER1$/check_ping -H $HOSTADDRESS$ -w 3000.0,80% -c 5000.0,100% -p 5"

checkLocalDisk :: Command
checkLocalDisk = Command "check-local-disk"
                         "$USER1$/check_disk -w $ARG1$ -c $ARG2$ -p $ARG3$"

checkLocalLoad :: Command
checkLocalLoad = Command "check-local-load"
                         "$USER1$/check_load -w $ARG1$ -c $ARG2$"

checkLocalProcs :: Command
checkLocalProcs = Command "check-local-procs"
                          "$USER1$/check_procs -w $ARG1$ -c $ARG2$ -s $ARG3$"

checkLocalUsers :: Command
checkLocalUsers = Command "check-local-users"
                          "$USER1$/check_users -w $ARG1$ -c $ARG2$"

checkLocalSwap :: Command
checkLocalSwap = Command "check-local-swap"
                         "$USER1$/check_swap -w $ARG1$ -c $ARG2$"

checkLocalMrtgtraf :: Command
checkLocalMrtgtraf = Command "check-local-mrtgtraf"
                             "$USER1$/check_mrtgtraf -F $ARG1$ -a $ARG2$ -w $ARG3$ -c $ARG4$ -e $ARG5$"

checkFtp :: Command
checkFtp = Command "check-ftp"
                   "$USER1$/check_ftp -H $HOSTADDRESS$ $ARG1$"

checkHpjd :: Command
checkHpjd = Command "check-hpjd"
                    "$USER1$/check_hpjd -H $HOSTADDRESS$ $ARG1$"

checkSnmp :: Command
checkSnmp = Command "check-snmp"
                    "$USER1$/check_snmp -H $HOSTADDRESS$ $ARG1$"

checkHttp :: Command
checkHttp = Command "check-http"
                    "$USER1$/check_http -I $HOSTADDRESS$ $ARG1$"

checkSsh :: Command
checkSsh = Command "check-ssh"
                   "$USER1$/check_ssh $ARG1$ $HOSTADDRESS$"

checkDhcp :: Command
checkDhcp = Command "check-dhcp"
                    "$USER1$/check_dhcp $ARG1$"

checkPing :: Command
checkPing = Command "check-ping"
                    "$USER1$/check_ping -H $HOSTADDRESS$ -w $ARG1$ -c $ARG2$ -p 5"

checkPop :: Command
checkPop = Command "check-pop"
                   "$USER1$/check_pop -H $HOSTADDRESS$ $ARG1$"

checkImap :: Command
checkImap = Command "check-imap"
                    "$USER1$/check_imap -H $HOSTADDRESS$ $ARG1$"

checkSmtp :: Command
checkSmtp = Command "check-smtp"
                    "$USER1$/check_smtp -H $HOSTADDRESS$ $ARG1$"

checkTcp :: Command
checkTcp = Command "check-tcp"
                   "$USER1$/check_tcp -H $HOSTADDRESS$ -p $ARG1$ $ARG2$"

checkUdp :: Command
checkUdp = Command "check-udp"
                   "$USER1$/check_udp -H $HOSTADDRESS$ -p $ARG1$ $ARG2$"

checkNt :: Command
checkNt = Command "check-nt"
                  "$USER1$/check_nt -H $HOSTADDRESS$ -p 12489 -v $ARG1$ $ARG2$"
