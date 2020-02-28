
module David.Queue (
    Stack(..),
    push,
    pop,
    peek,
    sfoldLeft,
    sreverse,
    sempty,
    len,
    list,

    Queue(..),
    qhead,
    qempty,
    enqueue,
    dequeue,
) where

-- | The 'Stack' type represents a functional FILO data structure.
data Stack a = Stack a (Stack a) | Empty deriving (Show, Eq)

push :: Stack a -> a -> Stack a
push s b = Stack b s

pop :: Stack a -> Stack a
pop (Stack a s) = s
pop Empty = badPop

badPop :: a
badPop = errorEmptyStack "pop"

peek :: Stack a -> a
peek Empty = errorEmptyStack "peek"
peek (Stack a s) = a

sfoldLeft :: (b -> a -> b) -> b -> Stack a -> b
sfoldLeft fn b0 Empty = b0
sfoldLeft fn b0 s = sfoldLeft fn (fn b0 (peek s)) (pop s)

sreverse :: Stack a -> Stack a
sreverse s = sfoldLeft push Empty s

sappend :: Stack a -> Stack a -> Stack a
sappend a b = sfoldLeft push b (sreverse a)

sempty :: Stack a -> Bool
sempty Empty = True
sempty _     = False

len :: Stack a -> Int
len s = sfoldLeft (\count _ -> count + 1) 0 s

list :: Stack a -> [a]
list s = reverse (sfoldLeft (flip (:)) [] s)

instance Semigroup (Stack a) where
    (<>) = sappend
instance Monoid (Stack a) where
    mempty = Empty


-- | The 'Queue' type provides a functional FIFO data structure.
--
-- The queue is implemented holding a front stack and a rear stack to provide 
-- easy access to the front and end of the queue.
--
-- Invariant of this functional Queue is that the front queue is never to be
-- empty unless the entire queue is empty. I.e. if the last element of the front
-- stack is popped, then the rear stack should be reversed and made available in
-- the front queue.
data Queue a = Queue (Stack a) (Stack a) deriving (Show, Eq)

qhead :: Queue a -> a
qhead (Queue Empty _) = errorEmptyQueue "qhead"
qhead (Queue s _) = peek s

qempty :: Queue a -> Bool
qempty (Queue Empty _) = True
qempty _               = False

enqueue :: Queue a -> a -> Queue a
enqueue (Queue Empty _) a = Queue (push Empty a) Empty
enqueue (Queue f r) a = Queue f (push r a)

dequeue :: Queue a -> Queue a
dequeue (Queue Empty _) = errorEmptyQueue "dequeue"
dequeue (Queue f r)
    | sempty (pop f) = Queue (sreverse r) Empty
    | otherwise      = Queue (pop f     ) r

qfoldLeft :: (b -> a -> b) -> b -> Queue a -> b
qfoldLeft fn b0 (Queue Empty _) = b0
qfoldLeft fn b0 q = qfoldLeft fn (fn b0 (qhead q)) (dequeue q)

qappend :: Queue a -> Queue a -> Queue a
qappend q r = qfoldLeft enqueue q r

instance Semigroup (Queue a) where
    (<>) = qappend
instance Monoid (Queue a) where
    mempty = Queue Empty Empty



-- Error code
------ Common strings to reduce constant strings when compiled (a la GHC.List)
errorEmptyStack :: String -> a
errorEmptyStack fun = error (module_str ++ fun ++ ": empty stack")

errorEmptyQueue :: String -> a
errorEmptyQueue fun = error (module_str ++ fun ++ ": empty queue")

module_str :: String
module_str = "David.Queue."
