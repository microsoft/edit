#!/bin/bash

# Numbers
42
3.14
-7
0xFF
0777
2#1010

# Constants
true
false

# Single-quoted strings
echo 'hello world'
echo 'it'\''s a trap'

# Double-quoted strings
echo "hello $USER"
echo "home is ${HOME}"
echo "escaped \$ and \" and \\"
echo "subshell: $(whoami)"
echo "arithmetic: $((1 + 2))"
echo "positional: $1 $# $? $! $@ $0 $$ $-"

# ANSI-C quoting
echo $'tab:\there\nnewline'
echo $'escape sequences: \a \b \e \f \r \t \v \\\\ \''

# Backtick interpolation
echo "today is `date +%Y-%m-%d`"

# Control flow
if [ -f /etc/passwd ]; then
    echo "exists"
elif [ -d /tmp ]; then
    echo "tmp exists"
else
    echo "neither"
fi

for i in 1 2 3; do
    echo "$i"
    continue
done

for ((i = 0; i < 3; i++)); do
    break
done

while true; do
    break
done

until false; do
    break
done

case "$1" in
    start) echo "starting" ;;
    stop)  echo "stopping" ;;
    *)     echo "unknown"  ;;
esac

select opt in "yes" "no" "quit"; do
    echo "$opt"
    break
done

time ls -la

# Test expressions
[ -f /etc/passwd ]
[ -d /tmp ]
[ -z "$var" ]
[ -n "$var" ]
[ "$a" = "$b" ]
[ "$a" != "$b" ]
[ "$a" -eq 1 ]
[ "$a" -ne 2 ]
[ "$a" -lt 3 ]
[ "$a" -gt 4 ]
[ "$a" -le 5 ]
[ "$a" -ge 6 ]
[[ "$name" == *.txt ]]
[[ "$name" =~ ^[0-9]+$ ]]
[[ -f /etc/passwd && -d /tmp ]]
[[ -f /etc/passwd || -d /tmp ]]

# Heredocs
cat <<EOF
Hello $USER
Your shell is $SHELL
EOF

cat <<'EOF'
No $interpolation here
EOF

cat <<-EOF
	Indented heredoc (tabs stripped)
EOF

# Herestrings
cat <<< "hello world"
cat <<< 'no interpolation'
cat <<< $name

# Variables
name="world"
echo $name
echo ${name}
echo ${name:-default}
echo ${name:=fallback}
echo ${name:+alternate}
echo ${name:?error}
echo ${#name}
echo ${name%.*}
echo ${name%%.*}
echo ${name#*/}
echo ${name##*/}
echo ${name/old/new}
echo ${name//old/new}
array=(one two three)
echo ${array[0]}
echo ${array[@]}
echo ${!array[@]}
echo ${#array[@]}

# Arithmetic
echo $((2 + 3))
echo $((a * b))
(( count++ ))
(( x = 5, y = 10 ))
(( x <<= 2 ))
(( x >>= 1 ))
(( x &= 0xFF ))

# Redirections
echo "out" > /dev/null
echo "append" >> /tmp/log
cat < /etc/passwd
echo "stderr" 2> /dev/null
echo "both" &> /dev/null
echo "dup" 2>&1
exec 3<> /tmp/fd3

# Process substitution
diff <(ls /bin) <(ls /usr/bin)
tee >(grep error > errors.log) > /dev/null

# Pipelines and logical operators
echo hello | cat
echo hello |& cat
ls && echo ok
ls || echo fail
sleep 10 &

# Subshells and group commands
(cd /tmp && ls)
{ echo one; echo two; }

# Extended globbing
shopt -s extglob
ls *.txt
ls ?(a|b)
ls *(a|b)
ls +(a|b)
ls @(a|b)
ls !(a|b)
echo ~

# Other keywords
export PATH="/usr/local/bin:$PATH"
local count=0
readonly PI=3
declare -a arr=(1 2 3)
declare -A map=([key]=value)
typeset -i num=42
alias ll='ls -la'
source /dev/null
. /dev/null

# Builtins
eval 'echo hello'
exec /bin/bash
trap 'echo bye' EXIT
test -f /etc/passwd
read -r line
printf "%s\n" "hello"
wait $!
kill -9 $$
jobs -l
fg %1
bg %1
cd /tmp
pwd
set -euo pipefail
unset name
shift
getopts "ab:" opt
command ls
builtin echo
type ls
hash -r
ulimit -n
umask 022
dirs
pushd /tmp
popd
disown %1
let "x = 1 + 2"
exit 0

# Functions
function greet() {
    local who="${1:-world}"
    echo "Hello, $who"
    return 0
}

cleanup() {
    echo "done"
}

greet "shell"
cleanup
