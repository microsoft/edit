#!/usr/bin/env fish

# Numbers
42
-7
3.14
.5
1e10
1.5e-3
0xFF
0xff
0777

# Constants
true
false

# Single-quoted strings
echo 'hello world'
echo 'it\'s a trap'

# Line continuation
echo this is a \
    continued line
set long_value one \
    two \
    three

# Double-quoted strings
echo "hello $USER"
echo "home is {$HOME}"
echo "escaped \$ and \" and \\"
echo "subshell: $(whoami)"
echo "legacy subshell: "(whoami)
echo "arithmetic: " (math 1+1)
echo "special paramters: $argv $status $fish_pid"
echo "positional: $argv[1] $argv[2..-1] $argv[-1]"

# Escape sequences
echo \x41 é \U0001F600 \e\[1m \a \b \f \r \t \v \n \cA
echo escaped\ space \(parens\) \{braces\} \* \? \~ \# \$ \& \; \| \< \>

echo 'first
# still quoted'; true
echo "first
# still quoted $USER"; false
echo "first\
continued $USER"; true
echo "escaped quote at EOL: \"
still quoted"; false
echo "escaped backslash at EOL: \\
still quoted"; true
echo 'backslash at EOL is literal: \
still quoted'; true
echo 'blank line follows

still quoted'; true

# Command substitution
set today (date +%Y-%m-%d)
set modern $(date +%Y-%m-%d)
echo (echo (echo nested (echo deeply)))
echo "today is "(date +%Y-%m-%d)" ISO 8601"
echo (echo first
echo second)
echo (printf '%s\n' a b c)[2]

# Comments
echo foo#bar
echo foo # trailing comment
echo "# not a comment"
echo '# not a comment'
echo \# not a comment

# Control flow
if test -f /etc/passwd
    echo "exists"
else if test -d /tmp
    echo "tmp exists"
else
    echo "neither"
end

for i in 1 2 3
    echo "$i"
    continue
end

for i in (seq 3)
    break
end

while true
    break
end

switch $argv[1]
    case start
        echo "starting"
    case stop
        echo "stopping"
    case '*.txt' '*.md'
        echo "text file"
    case '*'
        echo "unknown"
end

begin
    echo "group"
end

time ls -la

# Test expressions
test -f /etc/passwd
test -d /tmp
test -z "$var"
test -n "$var"
test "$a" = "$b"
test "$a" != "$b"
test "$a" -eq 1
test "$a" -ne 2
test "$a" -lt 3
test "$a" -gt 4
test "$a" -le 5
test "$a" -ge 6
test ! -f /etc/passwd
test -f /etc/passwd -a -d /tmp
test -f /etc/passwd -o -d /tmp
[ -f /etc/passwd ]
[ "$a" = "$b" ]
string match -q '*.txt' -- $name
string match -qr '^[0-9]+$' -- $name

# Input from pipes
printf '%s\n' one two | cat
echo hello | read -l line
cat /etc/hostname | string collect
echo 'no interpolation' | string upper
echo " hello world  " | string trim
sleep 10 &

# Variables
set name world
echo $name
echo {$name}
echo {$name}s
echo "$name"
echo "{$name}"
echo $name[1]
set -q name; or set name default
set -q name[1]
set -e name
set -l local_var 0
set -g global_var 1
set -x exported_var 2
set -gx PATH /usr/local/bin $PATH
set -U universal_var 3
set -S name
set array one two three
echo $array[1]
echo $array
echo $array[-1]
echo $array[2..3]
echo $array[-1..1]
echo (count $array)
echo $array[(count $array)]
set -a array four
set -p array zero
set -e array[1]
set ref array
echo $$ref
echo $$ref[1]

# Math
math 2 + 3
math "$a * $b"
math "2 ^ 8 + sqrt(16) * (3 - 1)"
math --scale=4 10 / 3
math --base=hex 255
math (math 1 + 2) \* 3
set count (math $count + 1)

# String builtins
string length -- $name
string sub -s 2 -l 3 -- $name
string split , -- "a,b,c"
string join , $array
string replace old new -- $name
string replace -ra '(\w+)@(\w+)' '$2:$1' -- "user@host"
string match -rg '^(\d+)-(\d+)$' -- "10-20"
string pad -w 8 -c 0 7
string repeat -n 3 ab
string escape -- 'a b'
string lower -- ABC

# Redirections
echo "out" > /dev/null
echo "append" >> /tmp/log
cat < /etc/passwd
echo "stderr" 2> /dev/null
echo "stderr append" 2>> /tmp/err
echo "both" &> /dev/null
echo "dup" 2>&1
echo "to stderr" >&2
echo "fd3" 3> /tmp/fd3
cat <&3
echo "noclobber" >? /tmp/maybe

# Process substitution
diff (ls /bin | psub) (ls /usr/bin | psub)
diff (ls /bin | psub -f) (ls /usr/bin | psub -s .txt)

# Pipelines and logical operators
echo hello | cat
echo hello &| cat
echo hello 2>| cat
ls && echo ok
ls || echo fail
ls; and echo ok
ls; or echo fail
not ls
! ls
sleep 10 &
true && false || true
begin; echo one; echo two; end | cat

# Globbing
ls *.txt
ls **/*.txt
ls ?.txt
ls file.{txt,bak}
ls {a,b{1,2},c}
ls ~
ls ~/Projects
ls ~root

# Keywords
set -gx EDITOR vim
set -l count 0
abbr -a gco 'git checkout'
abbr -a gst --set-cursor 'git status%'
alias ll='ls -la'
source /dev/null
. /dev/null
and echo chained
or echo chained
command ls
builtin echo
exec /bin/true

# Builtins
eval 'echo hello'
read -l line
wait $last_pid
kill -9 $fish_pid
jobs -l
fg %1
bg %1
cd /tmp
pwd
argparse 'h/help' 'v/verbose+' 'n/name=' -- $argv
type -q git
functions -q greet
functions -e greet
disown (jobs -lp)
umask 022
ulimit -n
history search --prefix git
random 1 100
realpath .
emit custom_event arg1 arg2
set_color -o red
set_color normal
commandline -f repaint
exit 0

# Functions
function greet
    set -l who $argv[1]
    test -n "$who"; or set who world
    echo "Hello, $who"
    return 0
end

function described --description 'A described function' --argument-names first second
    echo $first $second
end

function cleanup --on-event fish_exit
    echo "done"
end

function fish_prompt
    set -l last_status $status
    printf '%s> ' (prompt_pwd)
end

greet "shell"
cleanup

