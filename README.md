# Lambda Calculus Interpreter

This is a simple (for now) lambda calculus interpreter for use
in my programming language course.

It allows the user to enter a lambda calculus expression and
see it evaluated step by step.  It does alpha conversion
if necessary, but picking the new variable name is not very
sophisticated right now.

## Use

Clone the repository, use `stack repl` to get the Haskell
prompt, and `main` to start the lambda calculus repl.

## To do

Here are some things future versions will support if I get enough
requests (verbal requests or pull requests) for them:

  + Assigning variables
  + Loading and saving files
  + Change evaluation order
  + More sophistated variable name picking
  + Simple cycle detection
  + Statistics
