
module QueueTest (
    a,
    b,
    c,
    d,
) where

import David.Queue

a = Stack 5 (Stack 4 (Stack 3 (Stack 2 (Stack 1 Empty))))
b = foldl push Empty [1..5]
c = foldl push a [6..10]
d = sfoldLeft push b a
