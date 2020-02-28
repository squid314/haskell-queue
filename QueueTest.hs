
module QueueTest (
    a,
    b,
    c,
    d,
    e,
    f,
    g,
    h,
    i,
    p,
    q,
    r,
    s,
    t,
) where

import David.Queue

a = Stack 5 (Stack 4 (Stack 3 (Stack 2 (Stack 1 Empty))))
b :: Stack Int
b = foldl push mempty [1..3]
c = foldl push (pop (pop a)) [6..10]
d = sfoldLeft push b a
e :: Stack Int
f :: Stack Int
e = foldl push mempty [4..6]
f = foldl push mempty [7..9]
g =  b `mappend`  e  `mappend` f
h = (b `mappend`  e) `mappend` f
i =  b `mappend` (e  `mappend` f)
p :: [Int]
p = list i


q :: Queue Int
q = mempty
r = foldl enqueue q [1..3]
s = dequeue r
t = s `mappend` r `mappend` s
