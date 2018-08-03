-- Questão 07 - Implementar uma fila e seus algoritmos -- 

data Queue a = Queue [a] deriving Show

create = Queue([])

add x (Queue queue) = Queue (queue ++ [x])

remove (Queue queue) = (head(queue) , Queue(tail(queue)))

element (Queue queue) = (head(queue), Queue(queue))

peek (Queue queue) = head(queue)

poll (Queue queue) | ((length queue) == 0) = Nothing | otherwise = Just (head queue)