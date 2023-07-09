{ A simple example of a program }

"Hello, this prints the first n fibonacci numbers
How many numbers would you like to print?" print

slurp [i] ^
[ 1 i - [i] ^ ] [ decreaseI ] ^

1 [firstNumber] ^
1 [secondNumber] ^
[firstNumber secondNumber +] [nextFibo] ^
[firstNumber print] [printFirstFibo] ^

[ nextFibo! secondNumber [firstNumber] ^ [secondNumber] ^] [swapWithNextFibos] ^

[
 [ printFirstFibo ! :bye]
 [ printFirstFibo ! swapWithNextFibos ! decreaseI ! areWeThereYet ! ]
 i if ! ] [areWeThereYet] ^

areWeThereYet !