#!/bin/bash

case "$(uname -s)" in
    Darwin)
        localip=`ifconfig | grep "inet " | grep -Fv 127.0.0.1 | awk 'NR==1 {print $2}' `
        ;;
    Linux)
        localip=`ifconfig eth0|sed -n 2p|awk  '{ print $2 }'`
        ;;
    *)
        localip=`ifconfig eth0|sed -n 2p|awk  '{ print $2 }'`
        ;;
esac

echo $localip


#localip=`ifconfig $card|sed -n 2p|awk  '{ print $2 }'|awk -F : '{ print $2 }'`
