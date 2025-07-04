#!/usr/bin/env bash

# This is a comment

readonly VAR1="Hello"   # String literal
VAR2=42                 # Integer literal
VAR3=$((VAR2 + 8))      # Arithmetic expansion
VAR4=$(echo "World")    # Command substitution

function greet() {      # Function definition
    local name="$1"     # Local variable, parameter expansion
    echo "${VAR1}, $name! $VAR4"  # String, parameter expansion, variable
}

greet "User"            # Function call, string literal

if [[ $VAR2 -gt 40 && $VAR3 -eq 50 ]]; then  # Conditional, test, operators
    echo "Numbers are correct"   # String literal
elif (( VAR2 < 40 )); then       # Arithmetic test
    echo 'VAR2 is less than 40'  # Single-quoted string
else
    echo "Other case"
fi

for i in {1..3}; do     # Brace expansion, for loop
    echo "Loop $i"      # String, variable
done

case "$VAR4" in         # Case statement
    World) echo "It's World";;   # Pattern, string
    *) echo "Unknown";;          # Wildcard
esac

arr=(one two three)     # Array
echo "${arr[1]}"        # Array access

declare -A assoc        # Associative array
assoc[key]="value"
echo "${assoc[key]}"

# Here document
cat <<EOF
Multi-line
string with $VAR1
EOF

# Here string
grep H <<< "$VAR1"

# Subshell
(subshell_var=99; echo $subshell_var)

# Redirection
echo "Redirected" > /dev/null

# Background job
sleep 1 &

# Arithmetic assignment
let VAR2+=1

# Process substitution
diff <(echo foo) <(echo bar)

# Command grouping
{ echo "Group 1"; echo "Group 2"; }

# Escaped characters
echo "A quote: \" and a backslash: \\"

# End of file
