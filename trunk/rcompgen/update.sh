#!/usr/bin/env sh

## Usage: ./update.sh 
## 

## Has to be run in current directory.  Updates SvnLog [actually
## creates a file with latest entries]

## Note: -o is relatively new in grep
LASTLOG=`grep -o -m 1 "r[0-9]* |" SvnLog | sed -e 's/[^0-9]//g'`
svn log -v -r HEAD:${LASTLOG} > SvnLog.update

