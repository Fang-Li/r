#!/bin/bash

## 生成节点名称,以文件夹的名称命名,便携
if [ "$1" = "" ]; then
        local=`dirname $0`
        localip=`bin/ip.sh`
        NODE=`$local/dir.sh`
else
        NODE=$1
fi
echo $NODE
## 项目以eknife的节点名称接入远程节点调试
erl -name r$RANDOM@$localip -remsh $NODE@$localip -setcookie $NODE
