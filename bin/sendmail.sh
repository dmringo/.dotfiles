#!/bin/bash

# Expecting positional args:
#
#
# $1 : mu4e context name (same as mbsync group name)
# $2 : work/home - determines how we work around network issues
# $3 : host to connect to
# $4 : port to connect to - this determines whether TLS is used
#
# remaining args are passed to msmtp
#
# Example: scriptname lanl home smtp.gmail.com 587


usage() {
  local src="${BASH_SOURCE[0]}"
  sed -n -e "s|scriptname|$src|" -e "s|^#||" -e "2,/Example:/p" "$src"
  exit 1
}

ctx="$1"
shift || usage
loc="$1"
shift || usage
host="$1"
shift || usage
port="$1"
shift || usage

case "$loc" in
  home ) [ "$ctx" = "lanl" ] && ssh="ssh work-mac" ;;
  work ) [ "$ctx" != "lanl" ] && ssh="ssh vps" ;;
  * ) printf "Bad/missing loc arg: %s\\n" "$loc" ;;
esac

case "$port" in
  25 ) tls=off;;
  587 ) tls=on;;
  * ) printf "Bad/missing port arg: %s\\n" "$port" ;;
esac


# - tls-starttls is on by default, so not necessary
#
# - auto-from means that the caller should not use -f (set
#   message-sendmail-f-is-evil) instead using the envelope-from address
cmd="msmtp"
cmd="$cmd --host=$host"
cmd="$cmd --port=$port"
cmd="$cmd --tls=$tls"
cmd="$cmd --auto-from=on"
cmd="$cmd $@"

$ssh $cmd
