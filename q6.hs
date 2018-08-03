-- Questão 06 - Implementar uma pilha e seus algoritmos -- 

data Stack a = Stack [a] deriving (Show)

create = Stack([])

push x (Stack stack) = Stack (stack ++ [x])

pop (Stack []) = error "Não é possível realizar o pop de uma pilha vazia."
pop (Stack stack) = (last(stack), Stack(init(stack)))

empty (Stack stack) | (length stack == 0) = True
                    | otherwise = False

peek (Stack stack) = last(stack)

search x (Stack stack) = search' x stack 1
search' _ ([]) _ = (-1)
search' x (stack) index | last(stack) /= x =  search' x  (init(stack)) (index+1) | otherwise = index