{-
Реализация функции для вычисления числа Фибоначчи, 
основанная на прямом рекурсивном определении, крайне неэффективна - 
количество вызовов функции растет экспоненциально с ростом значения аргумента. 
GHCi позволяет отслеживать использование памяти и затраты времени на вычисление выражения, 
для этого следует выполнить команду :set +s:

GHCi> :set +s
GHCi> fibonacci 30
832040
(8.36 secs, 298293400 bytes)


С помощью механизма аккумуляторов попробуйте написать более эффективную реализацию, 
имеющую линейную сложность. Как и в предыдущем задании, 
функция должна быть определена для всех целых чисел.
-}

fibonacci :: Integer -> Integer
fibonacci n | n >= 0 = helperP 0 1 n
            | n < 0 = helperM  1 (-1) n
            
helperP accOld acc 0 = accOld
helperP accOld acc 1 = acc
helperP accOld acc n = helperP acc (acc + accOld) (n - 1)

helperM accOld acc (-1) = accOld
helperM accOld acc (-2) = acc
helperM accOld acc n = helperM acc (accOld - acc) (n + 1)