---
title: "A Lisp interpreter written in Lisp"
date: 2022-11-21 10:00:00 +0200
categories: lang
tags: lisp
math: true
---


A while ago I was recommended to watch [this talk](https://www.youtube.com/watch?v=OyfBQmvr2Hc&t=4157s) about Lisp, where a Lisp interpreter is presented with only several lines of Lisp code, as copied from [here](https://gist.github.com/lazywithclass/6af94f652cd59796e9592a5ea5772d17):

```scheme
(define eval-expr
  (lambda (expr env)
    (pmatch expr
      [`,x (guard (symbol? x))
        (env x)]
      [`(lambda (,x) ,body)
        (lambda (arg)
          (eval-expr body (lambda (y)
                            (if (eq? x y)
                                arg
                                (env y)))))]
      [`(,rator ,rand)
       ((eval-expr rator env)
        (eval-expr rand env))])))
```

`pmatch` is a pattern match function that evaluates the symbolic input `expr`, and determine whether it is a symbolic variable, a function or an function application, and `env` stores arguments that should be parsed to the function.
E.g., `((eval-expr '(lambda (x) (+ 1 x))) 5)` should return 6, because `(eval-expr '(lambda (x) (+ 1 x)))` should return a function, which takes argument `5` and return 6.
If without `env`, the incorrect `eval-expr` would return `(lambda (arg) (eval-expr '(+ 1 x)))`, here, `lambda` is no more symbolic, but `x` is still symbolic, and `eval-expr` does not know how to evaluate `x`.

With `env`, it does.
The `(eq? x y)` checks if the symbol given in the `body` is equal to the lambda argument provided in `expr`, e.g., it is in the case `(eval-expr '(lambda (x) (+ 1 x)))` but not in `(eval-expr '(lambda (x) (+ 1 y))')`.
Now when evaluating `x` with `eval-expr 'x`, the new `env` is also passed and `eval-expr` returns `(env x) = arg`, which means it is able to evaluate `(lambda (arg) (eval-expr '(+ 1 x)))` as `(lambda (arg) (+ 1 arg))`, which is now a normal function without any symbolic expression, hence it can accepts argument `5` and returns `6`.

The initialization of `env` does not matter, e.g., it can be `(eval-expr '(lambda (x) (+ 1 x)) (lambda (y) (error "unbounded symbol")))`.

