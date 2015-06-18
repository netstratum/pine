#!/bin/bash
rm /etc/init.d/pine
ret=false
getent passwd pine >/dev/null 2>&1 && ret=true
if $ret; then
	userdel pine
fi
