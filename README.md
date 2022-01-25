# Assessed Assignment 5

## Marking table

The exercises are defined so that it is hard to get a first-class mark.

```
  1st class   - 70 marks and above.
  upper 2nd   - 60-69 marks.
  lower 2nd   - 50-59 marks.
  third class - 40-49 marks.
  fail        -  0-39 marks.
```

## Preparation

In this assignment, we'll be extending the imperitive language `xy` of the [lecture notes](https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/tree/main/LectureNotes/Sections/interpreter) with some new features:

  * A monadic interpreter
  * Input/Output
  * A simple `for` iterator construct
  * Program Optimizations

We'll call our new language `xyio`.

The main directory of this week's assignment contains a skeleton
implementation of the new `xyio` language.  There are three template
files where you will make your changes.  In order to get started working on the assignment, you will need to

  * Copy the file `Parser-Template.hs` to `Parser.hs`
  * Copy the file `Interpreter-Template.hs` to `Interpreter.hs`
  * Copy the file `Optimization-Template.hs` to `Optimization.hs`

You will then implement your solutions in these new files.  Note that all the usual rules apply:

  * Do __not__ modify any of the other files in the directory.  We will be running you solutions
  with standard versions of these files so that any changes will be lost anyways and may cause you solution not to compile.

  * __Don't change the headers of the files listed above, including the module declaration, and, moreover, don't change the type signature of any of the given functions for you to complete.__

  * If you don't know how to complete an exercise, leave it as `undefined`, but do __not__ delete it!

  * __If you do make such changes, then we will not be able to mark your submission and hence it will receive zero marks!__


## Submissions should compile and run correctly on Jupyter Notebook

If your submission doesn't compile or run correctly on Jupyter Notebook, it will get zero marks.

## Submission procedure

* Run the presubmit script to be provided to you on your submission from Jupyter by running `./presubmit.sh` in the terminal (in the same folder as your submission).
* This will check that your submission is in the correct format.
* If it is, submit the **three** files `Parser.hs`, `Interpreter.hs` and `Optimization.hs` on Canvas.
* Otherwise fix and repeat the presubmission procedure.

## Plagiarism

Plagiarism will not be tolerated. Copying and contract cheating has led to full loss of marks, and even module or degree failure, in the past.

You will need to sign a declaration on Canvas, before submission, that you understand the [rules](https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/tree/main#plagiarism) and are abiding by them, in order for your submission to qualify.

## Question Difficulty

All questions have equal weight, but they are designed to increase in
difficulty. We have indicated an approximate difficulty level with
the 🌶️🌶 symbols according to the following scheme:

- 🌶 - Normal
- 🌶🌶 - Challenging
- 🌶🌶🌶 - Very challenging


## 🌶 Exercise 1 - Making the Interpreter Monadic

### Background - The `IO'` Monad

Since some of our new features will need to use input and output, we
will need to rewrite the main interpretation function `run` to be
monadic.  However, rather than using Haskell's built-in `IO` monad, it
will be convenient for marking purposes to use a custom version of
this monad which we call `IO'`.  The implementation of this monad can
be found in the file [IOPrime.hs](IOPrime.hs), though you do not need
to understand the implementation to complete the exercises on this
assignment.  Rather, you will only interact with the monad using the
standard monadic operations (`return`, `>>=`, and `do` notation ...)
as well as the handful of functions (`putChar'`, `getChar'`,
`purStr'`, `getLine'`, and so on) defined in `IOPrime.hs` which mimic
the similarly named functions defined in the Haskell Prelude.

See the Examples section below for information about how to run and
test your new implementation.

### Implementation Task

Rewrite the already existing clauses of the `run` function to use the
`IO'` monad.  You may wish to refer to the [original](https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/blob/main/LectureNotes/Sections/interpreter/Interpreter.hs) version to guide your implementation.

```haskell
run :: Program -> Storage -> IO' Storage

run (i := e)          m = undefined
run (IfElse e p q)    m = undefined
run (If e p)          m = undefined
run (While e p)       m = undefined
run (Block [])        m = undefined
run (Block (p : ps))  m = undefined
```

### Examples - Running the new Interpreter

The file `RunXY.hs` contains a top-level implementation which is compatible
with the new monadic interpreter.  It works in the same way as the
[command-line interface](https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/blob/main/LectureNotes/Sections/interpreter/Runxy.md) described in the lecture notes.  After completing
this first exercise, all the example files from the lecture notes should run
correctly:

```
$ runhaskell RunXY.hs factorial.xy 5
120
```

```
$ runhaskell RunXY.hs fibonacci.xy 6
8
```

## 🌶 Exercise 2 - Adding Input and Output

### Background

We'll begin by adding three input/output constructs to our language:

  * `read` - prompt the user for a number and bind it to a variable.
  * `write` - write the value of an expression to the console followed by a newline.
  * `print` - print a message provided by the user as a string followed by a newline.

The relevant part of the [language grammar](https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/blob/main/LectureNotes/Sections/interpreter/ConcreteSyntax.md) now has the additional lines:

```
Program ::= ......
          | read Identifier ;
          | write Expr ;
          | print String ;
```

### Implementation Task

1. Start by extending the parser to include the new syntax

    ```haskell
    readStatement = undefinedParse
	writeStatement = undefinedParse
	printStatement = undefinedParse
    ```

2. Now, extend the monadic `run` function from above to implement the desired operations using the `IO'` monad.

    ```haskell
	run (Read i)          m = undefined
    run (Write e)         m = undefined
    run (Print s)         m = undefined
	```
You can make use of any of the functions from the `IOPrime.hs` module, but in particular, you may want to look at `putStr'` and `getLine'`.

### Examples

Now that we have input and output capabilities, we no longer need to
fix an input variable `x` and an output variable `y` as we have
previously.  Our new `xyio` programs can be responsible for
initializing their storage and printing the important values
themselves.  To take advantage of these new features, we have included
a second top-level implementation called `RunIO.hs` which runs the program
it finds as-is.

For example, the file `factorial.xyio` rewrites the `factorial.xy` example to prompt the user for a number to compute the factorial of:
```
{
print "please input a number";
read x;
y := 1;
while (x > 0)
 {
 y := y * x;
 x := x - 1;
 }
print "its factorial is";
write y;
}
```
A sample session looks like this:
```
$ runhaskell RunIO.hs factorial.xyio
please input a number
6
its factorial is
720
```

## 🌶🌶 Exercise 3 - Adding a simple `for` iterator

### Background

Next, we'll add a simple construct for iterating over a range of
values.  For example, the user should be able to write
```
for (i <- 0 .. 10) {
  write i ;
}
```
to print the numbers between 0 and 10 (including 0 and 10).  The general form of our `for` iterator is
given by the following extension to the [language grammar](https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/blob/main/LectureNotes/Sections/interpreter/ConcreteSyntax.md)

```
Program ::= ......
          | for ( Identifier <- Expr .. Expr ) Program 
```

The corresponding piece of abstract sytax for this construct is found in the file `AbstractSyntax.hs` and looks as follows:

```hs
data Program = .....
             | For Identifier Expr Expr Program
```

To summarize, then: the `for` construct takes an identifier which will
be the iteration variable, two expressions which will determine the
initial and final values, and a program to run on each iteration. The
intended semantics is as follows:

1. The variable is initialized to the initial value.
2. The program is evaluated repeatedly, and after each evaluation,
   the variable is incremented by 1.
3. When the value of the variable exceds the max value, the loop stops.

In other words, our `for` loop is really just syntactic sugar for the following pseudo-code:
```
i := min;
while (i <= max) {
  { prog }
  i := i + 1;
}
```
where `i` is the identifier, `min` and `max` are the minimum and maximum values of the iteration, and `prog` is the program to run on each iteration.

### Implementation Task

1. Begin by extending the parser with the necessary code for the `for` construct:

    ```haskell
	forStatement = undefinedParse
	```

2. And now add the implementation to the `run` function:

    ```haskell
    run (For i min max p) m = undefined
	```

### Examples

The file `for.xyio` contains a simple iteration example.
```
{
  print "Enter a start value";
  read start;
  print "Enter an end value";
  read end;
  print "Here are your numbers:";
  for (i <- start .. end) {
    write i;
  }
}
```
When run from the command line, this produces:
```
$ runhaskell RunIO.hs for.xyio
Enter a start value
2
Enter an end value
4
Here are your numbers:
2
3
4
```

The file `factorial-for.xyio` contains a rewrite of the `factorial.xyio` example which uses this new construction:
```
{
print "please input a number";
read x;
y := 1;
for (i <- 1 .. x) {
  y := y * i;
}
print "its factorial is";
write y;
}
```
This has the same behavior as the original:
```
$ runhaskell RunIO.hs factorial-for.xyio
please input a number
7
its factorial is
5040
```

## 🌶🌶🌶 Exercise 4 - Optimizations

### Background

Many valid `xyio` program contain obvious simplifications which can make the program shorter.  For example, the user might write:
```
x := 7 + 6;
```
instead of just
```
x := 13;
```
Or the user may insert branches or loops which have no effect:
```
if (1) write 10; else write 20;
```
```
x := 0 ;
while (x) print "hello?" ;
```
Notice how in the last example, even though the expression evaluated by the `while` loop is a variable, we can nonetheless see from the program structure that this variable is *constant*, and so the `while` loop has no effect and can safely be removed.  This is sometimes called *dead code elimination*.

The goal of this exercise will be to detect such simplifications and
transform the user's program to remove them.

As there are many, many possible subtly different implementations, we will leave
this exercise somewhat open-ended.  You may use whatever technqiues you like,
and make whatever simplifications you can think of.

__NB: Your simplifications are subject, of course, to the condition that the
resulting program behaves the same as the original.__

Your grade will be based on how much smaller your resulting program is
when compared with the original.  The more simplifications you are
able to make, the higher your mark (though we will give you some
suggestions and examples below so that you have a rough idea of what kinds
of simplifications are possible).

### Implementation Task

Write an optimization function
```haskell
optimize :: Program -> Program
optimize = undefined
```
which implements as many code-size-reducing optimizations as you can find, but in particular, includes expression simplification and dead-code elimination.

Although you are free to complete this exercise in the generality just given, we are going to suggest a simple setup to get you started.  Feel free to take advantage of it if you wish.

The central idea of this setup is that the crucial information when doing these sorts of optimizations is to know *which variables are known to be constant*.  Let us make the following definition to capture this idea:

```hs
type OptStorage = Identifier -> Maybe Integer
```
This type is similar to the `Storage` type used by the iterpreter: it maps identifiers to values.  In this case, however, if `o :: OptStorage` and `o "i" == Just 7`, we interpret this as saying that the variable `i` is known to be constant with value `7`.  The fact that the function returns a `Maybe Integer` gives us the additional possibility to *erase* identifiers from this list of constants, for example using the following function:
```hs
deleteVar :: Identifier -> OptStorage -> OptStorage
deleteVar i m j | i == j    = Nothing
                | otherwise = m j
```
This is important since variables which are known to be constant in some parts of the code may become non-constant later on.  Consider, for example:
```
{
  x := 0;
  if (x) write 7;
  read x;
  if (x) write 10;
}
```
Clearly `x` should be among the variables known to be constant when we consider the first `if` expression, so that it can be safely removed.  However, right after this, we `read` to `x`, at which point we lose track of its value so that the second `if` cannot be removed.

So with this in mind, the setup we suggest is the following: think of program optimization as a stateful computation, whose state is an `OptStorage` which keeps track which variables are known to be constant.  This point of view further suggests that we break our optimization function into one for `Expr`'s and another for `Program`'s.  We wish, therefore, to implement the following two functions:
```hs
optExpr :: Expr -> State OptStorage Expr
optExpr = undefined

optProgram :: Program -> State OptStorage Program
optProgram = undefined
```
These function will clearly depend on each other: programs contain expressions, and the state of the optimization of a program determines which expressions are constant.

The original optimization task can then be defined by simply running these computations starting from the empty list of constants:
```hs
optimize p = fst $ runState (optProgram p) emptyOptStorage
```

We have implemented this basic setup, along with some utility functions in the `Optimization-Template.hs` file.  Feel free to make use of them as you like.

### Examples

We have included a top-level module `RunOptimize.hs` which you can use
to test your optimization routines.  Because optimization uses the `State` monad,
this file needs to be run with special arguments to `runhaskell`.
```
runhaskell --ghc-arg="-package mtl" RunOptimize.hs <filename>
```
To make this easier, there is a script called `optimize.sh` in the assignment directory which
will run your optimizer directly on a provided filename.  So, for example the file `op-ex0.xyio`
contains the following simple program:
```
x := 7 + 6;
```
We can see the output of running our optimizer as follows:

```
$ ./optimize.sh op-ex0.xyio
x := 13;
```

We'll now go through a number of examples of optimizations, all of which are handled by the reference implementation in order to give you an idea of what is possible.

#### Expression Simplification

Expressions which contain constant sub-expressions or contain instances of any of the following algebraic identities may be simplified and rewritten.

```
          x + 0 = 0 + x = x
          x * 1 = 1 * x = x
          0 * x = x * 0 = 0
          1 && x = x && 1 = x
          0 && x = x && 0 = 0
          1 || x = x || 1 = 1
```

The file `op-ex1.xyio` contains a number of these types of examples:

```
{
  a := 7 + 6 ;
  b := (10 * 3) + c ;
  d := e + (10 - 10);
  f := (7 * 0) * g ;
  h := 1 || (i + j) ;
  k := (k / 14) && 0 ;
}
```
Running our optimizer on this input gives:
```
$ ./optimize.sh op-ex1.xyio
{
  a := 13;
  b := 30 + c;
  d := e;
  f := 0;
  h := 1;
  k := 0;
}
```
Notice how expressions are simplified when they appear on the right-hand side of assignment statements.


#### Simplifying Blocks

Empty sub-blocks may be removed from their enclosing blocks, as can blocks which
contain just a single entry (leaving the entry in the parent block). Examples of
this type can be found in `op-ex2.xyio`:

```
{
  x := 0;
  { }
  { y := 10; }
}
```
```
$ ./optimize.sh op-ex2.xyio
{
  x := 0;
  y := 10;
}
```

#### Eliminating trivial `if` statements

If the condition appearing in an `if` or `if-else` statement is known
to be true or false, then the appropriate branch may be chosen
automatically, or the `if` statement may be removed if appropriate. The
file `op-ex3.xyio` contains some examples:

```
{
  x := 12;
  y := x - 12;

  if (y) z := 14;

  if (x)
    w := 27;
  else
    w := 41;
}
```
Running the optimizer, we find:
```
$ ./optimize.sh op-ex3.xyio
{
  x := 12;
  y := 0;
  w := 27;
}
```

#### Simplifying `while` and `for` loops

A `while` loop whose condition is known to be false may be removed.  Similarly, a `for` loop for which the maximum value is known to be strictly smaller than the minimum value can also be removed.  The file `op-ex4.xyio` contains some examples of these conditions.

```
{
  x := 14;
  while (x / 2 > 9) {
    write x;
  }
  for (i <- x .. 7) {
    print "can you hear me?";
  }
}
```
In the above example, both loops may be removed:
```
$ ./optimize.sh op-ex4.xyio
x := 14;
```
