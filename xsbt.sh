#!/bin/bash
SCRIPT_PATH="${BASH_SOURCE[0]}";
if([ -h "${SCRIPT_PATH}" ]) then
  while([ -h "${SCRIPT_PATH}" ]) do SCRIPT_PATH=`readlink "${SCRIPT_PATH}"`; done
fi
pushd . > /dev/null
cd `dirname ${SCRIPT_PATH}` > /dev/null
SCRIPT_PATH=`pwd`;
popd  > /dev/null

script_dir=`cygpath -w ${SCRIPT_PATH} || echo ${SCRIPT_PATH}`

java $SBT_OPTS -Xmx512M -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -jar $script_dir/sbt-launcher/xsbt-launch.jar "@xsbt.boot.properties" "$@"