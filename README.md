Задание первой недели курса Functional Programming Principles in Scala (Scala 2 version) с сайта coursera.org.

Необходимо реализовать три функции.  
Первая из функций, вычисляет элементы треугольника Паскаля с помощью рекурсивного процесса.

треугольником Паскаля:  
.....1  
....1 1  
...1 2 1  
..1 3 3 1  
.1 4 6 4 1  
...........  

Все числа на краю треугольника равны 1, а каждое число внутри треугольника представляет собой сумму двух чисел над ним.
 
Например, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
 
-------------------------------------------------------------------------------  
Вторая функция рекурсивно проверяет балансировку скобок входной строки. 

Например, для данных строк должно возвращаться true:  

 (if (zero? x) max (/ 1 x))

 I told him (that it’s not (yet) done). (But he wasn’t listening)

Для данных -- false:  
 :-)
 
 ())(
 
---------------------------------------------------------------------------------  
Третья функция рекурсивно подсчитывает количество различных способов для получения суммы, имея список номиналов монет.

Например, есть 3 способа получить 4, если у вас есть монеты достоинством 1 и 2:  
  1. 1 + 1 + 1 + 1,  
  2. 1 + 1 + 2,  
  3. 2 + 2.
