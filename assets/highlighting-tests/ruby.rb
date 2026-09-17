# Single-line comment

=begin
Multi-line comment
=end

=begin rdoc
The rest of the `=begin` line is ignored by Ruby.
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
1.upto(3)
(1..3).to_a
(1...3).to_a

# Constants
true
false
nil
Object
String
Demo::Animal
::Kernel
MAX_SIZE = 10

# Strings
'single quotes with escape: \' \n \t \\'
"double quotes with escape: \" \n \t \\"
"interpolated: #{1 + 1} and a # that is not a comment"
"escaped \#{1 + 1} is not interpolated"
`echo shell command`
`echo #{Time.now}`

# Symbols and variables
:symbol
:method_name?
attr_accessor :name, :age
@instance_var
@@class_var
$global_var
{ key: 'value' }

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

until true
  puts "spin"
end

begin
  raise "oops"
rescue StandardError => e
  retry
ensure
  puts e
end

puts "even" if 42.even? and not false
puts "odd" or true

# Definitions and method calls
module Demo
  class Animal
    def initialize(name)
      @name = name
    end

    def self.create(name)
      new(name)
    end

    def ==(other)
      @name == other.name
    end

    def name=(value)
      @name = value
    end

    def speak! = puts "#{@name} speaks"
  end
end

alias old_speak speak!
undef old_speak

BEGIN { puts "start" }
END { puts "finish" }
defined? Demo
puts __FILE__, __LINE__
self
super
yield

puts "hello"
Array.new(3)
greet("world")
