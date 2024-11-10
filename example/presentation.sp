["




























































  " ,] [newPage] @
[(slurp  newPage ! )] [waitForCharacter] @

[:s] [results] @

[ print waitForCharacter ! ] [printThenWait] @
"Hello and welcome to TechDive presentation of spacelang" ,
waitForCharacter !

"What is spacelang you ask?" ,
waitForCharacter !

"It is a stack based programming language (akin to Forth), that: " , waitForCharacter !
"Pause for dramatic effect." , waitForCharacter !
"- is simpler (?)," , waitForCharacter !
"- proposes a novel async model (as far as stack languages go)," , waitForCharacter !
"- leverages lazy evaluation," , waitForCharacter !
"- has some interesting features," , waitForCharacter !
"- meta-programming is built-in and part of the normal functioning of the language." , waitForCharacter !

"We have a stack to which we can push Terms:
We simply push them by writing them down:" ,

"> 1 2 " . waitForCharacter !

"In this example the interpreter reads '1' pushes it to the stack, then reads
'2' and pushes it to the stack resulting in:" . waitForCharacter !
1 2 results ! waitForCharacter !

"The evaluation model of the language is a key aspect of what makes it special.
Left to right, sequential evaluation ensures that our programs are confluent,
even with the addition of effects." .

waitForCharacter !

"Our terms can be numbers, strings, booleans, words, or thunks." ,
"> 1 'strings' true + [thunk]" ,
waitForCharacter !

"Words, and terms in spacelang are from a type perspective arrows, which means
that they compose. Let's use the following notation:
  - '(input -- output)'
  - capitalised for defined types
  - uncapitalised for variables

  Therefore we have
1 : ( -- Number)
+ : ( Number Number -- Number)
  etc.

  This 'arrow' property of terms means that any term is a function, and it also
  gives us the concatenative property of the language. In essence a sequence of
  two terms is a composition." , waitForCharacter !

"As the TechDive is a bit short, to cover 'Principia Mathematica', we'll accept
that numbers are primitives that evaluate to themselves. As seen before. So are strings.

From a type perspective we say
'a' : ( -- String)
1   : ( -- Number)
  " ,
waitForCharacter !

"The first notable exception are words. Words evaluate to their binding (whatever
that is). The language provides some built in words:
  - numerical operations: + - / * < <= etc.
  - machine operations: :h :r :debug :m : rs etc.
  - memory operations: @
  - evaluator operations: !
  - inter-machine operations: $",
waitForCharacter !

"But the funkiest one is the thunk which we write by enclosing a term within
square brakets, like so:
  > [1 3 +]

  Which has the type:
  [1 3 +] : ( -- ( -- Number))

  So what's going on here?
  ", waitForCharacter !

"We force evaluation of a thunk with the operator '!', which behaviour we can
describe with the term

  ((a) -- a)
",
waitForCharacter !

"We can bind terms to words with the word `@`, together with thunks we can now
define our own funky words.

  > [1 +] [addOne] " ,
waitForCharacter !

"Word bindings have a notion of scope, therefore if we do the following:

  > ( [1 +] [addOne] 2 addOne ) 4 addOne

The program will crash and burn as addOne is not defined/bound outside the scope
defined by the parens.

As the parens themselves are terms - we can use scope local to our thunks as so
  >  [ ([x] @) ]
  A term which would pop x and discard it essentially.
  " , waitForCharacter !

"Last thing to mention is that spacelang is more or less a language to
  orchestrate a machine. Imagine many such machines each with their own address.
  Here is where $ operator comes into play.
  This is the async model of space.
  " , waitForCharacter !

"Enough talking, let's find bugs!" , waitForCharacter !

"The End." , waitForCharacter !
"*Pause for aplause.*" , waitForCharacter !

"Any questions?" ,
