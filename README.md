# infix-lisp

This is infix-lisp, an esoteric functional programming language with only infix, binary functions.

Despite the name, infix-lisp is not a lisp at all!
(in fact, it does not have a list primitive, but uses tuple-based linked lists instead.)

## Examples

### Basics

- Basic calculations
  ```lisp
  (1 + 5)
  (5 - 10)
  ((1 + 5) * 2)
  ```
- A lambda
  ```lisp
  [a (a + b) b]
  ```
- Conditionals
  ```lisp
  ("everything is fine" <(1 < 10)> "Something is very wrong here")
  ```
- Input and output
  ```lisp
  (_ print ("Hello, " ++ (_ readString _)))
  ```

### A simple FizzBuzz

This example shows an implementation of FizzBuzz.

The `map` and `:` functions are part of the standard library, so in actual infix-lisp code you would not have to define these.

```lisp
(
  { fizzBuzz =
      [_
        (
          (nil print "FizzBuzz")
        <(((num % 3) == 0) && ((num % 5) == 0))>
          (
            (nil print "Fizz")
          <((num % 3) == 0)>
            (
              (nil print "buzz")
            <((num % 5) == 0)>
              (nil print num)
            )
          )
        )
      num]

  , map =
      [tuple
        (
          { tail = (_ snd tuple) , }
        in
          (
            (_ f (_ fst tuple))
          ,
            ((tail map f) <(tail != nil)> nil)
          )
        )
      f]

  , : =
      [lower
        (lower , (((lower + 1) : upper) <(lower < upper)> nil))
      upper]
  , }
in
  ((1 : 100) map fizzBuzz)
)
```


### a quine
```lisp
( { x = 
    [_ 
      ( 
        _
      print 
        ( 
          "( { x = " 
        + 
          ( 
            (_ toString x) 
          + 
            " } in ( _ x _ ) ) "
          ) 
        )
      )
    _] 
  } 
in 
  (_ x _)
)

```

## Syntax

The syntax of infix-lisp is actually quite simple,... it's just also very ugly.
Have fun reading this!

### Literals

Infix-lisp supports string, integer and list literals.

- String literals are strings surrounded by either `"` or `'`
- Integer literals work as you would expect (`12`)
- list literal syntax is values seperated by commas, surrounded by `{}`, i.e.: `{ 1, 2, 3, "hi", "ho", 5 }`

### Function invocation

A lot of things, including many symbols, are valid identifiers and thus function names in infix-lisp.
Invoking a function is as simple as putting it in parentheses, between it's two arguments, for example `(1 + 5)`.

Note that the spaces around the function name as well as the parentheses are required here.

### Lambdas

A lambda takes the shape `[argName1 body argName2]`, where `body` can be any piece of code, i.e.: `[a (a + b) b]`.

If you want to define a function with a name, you'll have to bind a lambda to a variable.

### Bindings

Everything in infix-lisp has to evaluate to some value. Thus, you cannot define top-level variables, as these would not themselves evaluate to anything.

To define bindings, you use the `({ name = value, name2 = value2 } in block)` syntax, which will make the defined variables available to use in the block.
Example:

```lisp
(
  { number1 = 1
  , number2 = 2
  , plus = [a (a + b) b]
  }
in
  (number1 plus number2)
)
```

Note that in this example, `plus = +` would have been equally valid.

### Conditionals

In infix-lisp, there is no such thing as an `if` without an `else`.

The syntax for conditionals is `(yesBody <condition> noBody)`, where `yesBody`, `condition` and `noBody` can all be arbitrary expressions.
The `condition` expression has to evaluate to a boolean.
Examples:

```lisp
("hi" <(1 == 12)> "ho")
("hi" <true> "ho")
```

### Tuples

A tuple can be created using the `,` function, i.e.: `(1 , 2)`.
The values of that tuple can be accessed with the `fst` and `snd` functions:

```lisp
((nil fst (1 , 2)) == 1)
((nil snd (1 , 2)) == 2)
```

### Lists

Infix-lisp does not have a built-in list primitive. Instead, you just use tuples to build up a linked list:

```lisp
(1 , ( 2 , ( 3 , ( 4 , nil ) ) ))
```

As good ergonomics are obviously one of the biggest design goals of infix-lisp,
you can also use the

```lisp
{ 1, 2, 3, 4 }
```

syntax to create the same value.
