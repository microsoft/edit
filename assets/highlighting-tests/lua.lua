-- Comments
-- Single-line comment

--[[
Multi-line comment
]]

--[=[
Extended multi-line comment
]=]

-- Numbers
local n1 = 42
local n2 = 3.14
local n3 = 0.5
local n4 = 1e10
local n5 = 1.5e-3
local n6 = 0xff
local n7 = 0xFF
local n8 = 0x1f

-- Constants
local c1 = true
local c2 = false
local c3 = nil

-- Strings
local s1 = 'single quotes with escape: \' \n \t \\'
local s2 = "double quotes with escape: \" \n \t \\"

local s3 = [[
Multi-line string
]]

local s4 = [=[
Extended multi-line string
]=]

-- Control flow keywords
if true then
elseif false then
else
end

for i = 1, 10 do
    if i == 5 then
        break
    end
end

while false do
end

repeat
until true

-- Other keywords
local x = 1
local _y = not false and true or nil
local value = "sample"
local items = { 1, 2, 3 }
local object = {
    method = function(self)
        return self
    end,
}

function greet(name)
    return "Hello, " .. name
end

local function helper()
    return x + n1
end

type Point = {
    x: number,
    y: number,
}

export type Callback = (value: string) -> ()

local kind = typeof(value)

for _, item in pairs(items) do
    print(item)
    continue
end

-- Function calls
print("hello")
greet("world")
table.insert(items, 42)
object:method()

print(n1, n2, n3, n4, n5, n6, n7, n8)
print(c1, c2, c3)
print(s1, s2, s3, s4)
print(kind)
print(helper())
