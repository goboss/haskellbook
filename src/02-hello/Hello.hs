module Hello where

z = 7

y = z + 8

x = y ^ 2

waxOn = x * 5

-- Exercise 4: Rewrite waxOn as an expression with a where clause in your source file.
waxOn' = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

-- Exercise 5: To the same source file where you have waxOn, add the triple function.
triple x = x * 3

-- Exercise 6: Now, without changing what you’ve done so far in that file,
-- add a new function called waxOff that looks like this:
waxOff x = triple x

