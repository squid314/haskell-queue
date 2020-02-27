
module David.Queue (
    Stack(..),
    push,
    pop,
    peek,
    sfoldLeft,
    sreverse,
    len,
) where

data Stack a = Stack a (Stack a) | Empty deriving (Show, Eq)

push :: Stack a -> a -> Stack a
push s b = Stack b s

pop :: Stack a -> Stack a
pop (Stack a s) = s
pop Empty = badPop

badPop :: a
badPop = errorEmptyStack

peek :: Stack a -> a
peek (Stack a s) = a

sfoldLeft :: (b -> a -> b) -> b -> Stack a -> b
sfoldLeft fn b0 Empty = b0
sfoldLeft fn b0 (Stack a s) = sfoldLeft fn (fn b0 a) s

sreverse :: Stack a -> Stack a
sreverse s = sfoldLeft (push) Empty s

len :: Stack a -> Int
len s = sfoldLeft (\count _ -> count + 1) 0 s



-- | The 'Queue' type provides a functional FIFO data structure.
data Queue a = Queue (Stack a) (Stack a) deriving (Show, Eq)



--instance Semigroup Queue a where
--    (<>) = (++)
--instance Monoid Queue a where
--    mempty = Queue Empty Empty
--
--head :: Queue a -> a
--head (Queue (Stack a _) _) = a
--
--enqueue :: Queue a -> a -> Queue a
--enqueue (Queue Empty Empty) a = Queue (push Empty a) Empty
--enqueue (Queue f r) a = Queue f (push r a)
--
--dequeue :: Queue a -> Queue a
--dequeue (Queue (Stack _ Empty) r) = Queue (reverse r) Empty
--dequeue (Queue f r) = Queue (pop f) r
--
--
--



-- Error code
------ Common strings to reduce constant strings when compiled (a la GHC.List)
errorEmptyStack :: String -> a
errorEmptyStack fun = error (module_str ++ fun ++ ": empty stack")

errorEmptyQueue :: Stirng -> a
errorEmptyQueue fun = error (module_str ++ fun ++ ": empty queue")

module_str :: String
module_str = "David.Queue."
