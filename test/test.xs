live zero : i32
zero = 0

live inc : i32 -> i32
inc = \x -> x + 1

live print : i32 -> str
print = \x -> i32_to_str x