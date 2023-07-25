{ A simple example of a program }

{ We first print a greeting }
"Hello, this prints the first n fibonacci numbers
How many numbers would you like to print?" print

{ slurp the amount of fibos we want to print }
slurp [i] ^

{ define a word for decrease i by one }
[ 1 i - [i] ^ ] [ decreaseI ] ^

{ define our first number and second number }
1 [firstNumber] ^
1 [secondNumber] ^

{ define a word for creating the next fibo number }
[firstNumber secondNumber +] [nextFibo] ^

{ and a word for printing the first fibo number (of the two stored) }
[firstNumber print] [printFirstFibo] ^

{ define a way to swap first and second fibo stored numbers }
[ nextFibo! secondNumber [firstNumber] ^ [secondNumber] ^] [swapWithNextFibos] ^

{ then we define our main loop }
[[ printFirstFibo ! :bye ]
 [ printFirstFibo ! swapWithNextFibos ! decreaseI ! areWeThereYet ! ]
 i if ! ]
[areWeThereYet] ^

{ and finally, call it }
areWeThereYet !