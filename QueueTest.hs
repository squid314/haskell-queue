
module TestQueue (
    a,
    b,
) where

import Testing.Queue

a = Stack 5 (Stack 4 (Stack 3 (Stack 2 (Stack 1 Empty))))
b = foldl 
