5 [i] ^                                   { i is our index indicating how many fibo numbers we want to print }
[1 i - [i] ^ ] [decreaseI] ^              { function to decrease i }

1 [firstNumber] ^                         { bind the first number }
1 [secondNumber] ^                        { bind our secodn number }
[firstNumber secondNumber +] [nextFibo] ^ { define our word for creating the next fibo number }
[firstNumber .] [printFirstFibo] ^        { define a word for printing the first fibo }

{now we define a word for swapping 1st number with second and second with next}
[ secondNumber [firstNumber] ^
  nextFibo! [secondNumber] ^ ] [swapWithNextFibos] ^


{and let's create a word for checking if we can stop}

[[ printFirstFibo ! swapWithNextFibos ! decreaseI ! areWeThereYet ! ] {else case goes first}
 [ printFirstFibo !] { happy path goes second }
 i if] [areWeThereYet] ^

areWeThereYet !