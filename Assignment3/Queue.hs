-- |
-- = A Module for FIFO Queues.
module Queue where

    -- | A queue is represented by a pair of lists, with the intended meaning the
    -- the first list is the first half of the queue and the second list is the
    -- second half of the queue in reversed order. That is, given a queue @(xs,
    -- ys)@, the \"content\" of the queue is @xs ++ reverse ys@. This representation
    -- allows reasonably efficient access to both the head and the tail of a queue.
    type Queue a = ([a], [a])
    
    -- | The empty queue.
    empty :: Queue a
    empty = ([], [])
    
    -- | Test whether a queue is empty.
    isEmpty :: Queue a -> Bool
    isEmpty ([], []) = True
    isEmpty (_, _) = False
    
    -- | Enqueue an element at the end of the queue.
    enqueue :: a -> Queue a -> Queue a
    enqueue x (xs, ys) = (xs, x:ys)
    
    -- | Dequeue the first element of the queue. The result pair consists of this
    -- element together with the remaining queue.
    dequeue :: Queue a -> (a, Queue a)
    dequeue (x:xs, ys) = (x, (xs, ys))
    dequeue ([], ys)
      | null ys = error "empty queue"
      | otherwise = (z, (zs, []))
        where
          z:zs = reverse ys