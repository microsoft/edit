# Comments
# Single-line comment

=begin
Multi-line
comment
=end

# Numbers
42
3.14
0.5
1e10
1.5e-3
0xff
0xFF
0b1010
0o77
1_000_000
3.14r
2i

# Constants
true
false
nil
Object
String

# Strings
'single quotes with escape: \' \n \t \\'
"double quotes with escape: \" \n \t \\"
`echo shell command`

# Symbols and variables
:symbol
:method_name?
@instance_var
@@class_var
$global_var

# Control flow keywords
if true
  puts "yes"
elsif false
  puts "no"
else
  puts "maybe"
end

unless nil
  puts "not nil"
end

case 42
when 1
  puts "one"
else
  puts "other"
end

for i in 1..3
  next if i == 2
  break if i == 3
end

while false
  redo
end

begin
  raise "oops"
rescue StandardError => e
  retry
ensure
  puts e
end

# Definitions and method calls
module Demo
  class Animal
    def initialize(name)
      @name = name
    end

    def speak! =  puts "#{@name} speaks"
  end
end

alias old_speak speak!
undef old_speak

BEGIN { puts "start" }
END { puts "finish" }
defined? Demo
self
super
yield

puts "hello"
Array.new(3)
greet("world")
