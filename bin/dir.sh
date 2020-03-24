#!/bin/bash

# 脚本所在相对路径
local=`dirname $0`

# 脚本所在全路径
path=$(dirname $0)
if [ ${path:0:1} == . ]; then
    path=${path/./$(pwd)}
fi

# 脚本所在目录的上级目录名称
parent_dir=`echo $path | rev| cut -d / -f 2 | rev`
echo $parent_dir


# NODE=${PWD##*/} ## 获取文件夹的名称,作为节点名称