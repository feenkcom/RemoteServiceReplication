#! /bin/bash
### Install RSR from Tonel files into a Rowan-enabled stone
### Exits with 0 if success, 1 if failed

rsrHome=${ROWAN_PROJECTS_HOME}/RemoteServiceReplication
## Topaz refuses to exit from script if input is stdin, so redirect from /dev/zero
topaz -I ${rsrHome}/scripts/login.topaz  -S ${rsrHome}/src-gs/loadRSR.topaz < /dev/zero
if [ $? = 0 ]
    then
        exit 0
    else
        echo !!!!!!!!!!!!!!
        echo INSTALL FAILED
        echo !!!!!!!!!!!!!!
        exit 1
    fi
