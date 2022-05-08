---
title: Continuation
date: 2015-03-11 11:54:17
tags:
- lisp
- PL
categories:
- articles
---

# continuation

这是一份关于continuation的[文档](http://www.cs.unm.edu/~williams/cs491/appcont.pdf),
将continuation讲的非常清楚,我简要的总结下我的理解: 先说几个概念与符号:

1.  escape procedure: 它和普通的procedure是一样的,而且也有相同的行为,会返回相
    同的值,唯一的不同的是这个函数它返回后就会替换调用栈,也就是说它会返回解释器
    的最顶层或者解释器的REPL循环, 一个普通的procedure对应的escape procedure通 过在符号后添加^ 来表示,比如
    `k` 是一个普通的procedure, 那么它对应的escape procedure就是 `k^` , 还有 `lambda`
    是用来构建普通的procedure, 那么 `lambda^` 就是用来构建 `escape
    procedure`.(注意 `lambda^` 不是scheme的一 部分)下面举个例子:
    
    ``` scheme
    (define k
      (lambda (x y)
        (+ x y)))
    ```
    
    现在假设你使用 `(+ 3 (k 1 2))` 那么结果是6, 但是如果你使用 `(+ 3 (k^ 1
            2))` 它就会返回 `3`, 原因是 `k^` 与 `k` 虽然有相同的返回值,但是它会替换 调用栈,所以也就不会执行后面的
    `(+ 3)`,而是直接返回 `k^` 的值

2.  continuation: 通俗点说 `continuation` 实际就是代表接下来要做的事或要进行的 操作, 也就是所谓的 `the
    rest of computation`, 所以当你要找一个函数来代表
    某一点的continuation时,你只要弄清楚该点接下来要进行的操作,把这些操作封装
    进一个函数就好 有了 `escape procedure` 的概念后,那么continuation实际就是 一个escape
    procedure.

3.  scheme的 call/cc
    
    ``` scheme
    (+ 2 (call/cc
          (lambda (k^)
            (* 5 (k^ 4)))))
    ```
    
    从上例可以看出 `call/cc` 的参数是一个lambda函数, 该函数也有一个参数(`k^`), 很显然 `k^`
    是一个escape procedure, 当然 `k^` 也代表当前的
    `continuation`,在上例中,它的定义可以大致认为是这样的:
    
    ``` scheme
    (lambda^ (v)
             (+ 2 v))
    ```
    
    因为上例直接返回最顶层,所以可以直接这样
    
    ``` scheme
    (lambda (v)
      (+ 2 v))
    ```
    
    所以当你使用 `(k^ 4)` 时, 它就返回 `6`, 同时替换调用栈,返回解释器的最上层,同
    时从这个例子你也可以体会continuation的含义,
    continuation就是接下来要做的 事或者操作, 那么上例中 `call/cc` 之后接下来要做的事显然就是 (+
    2 ret-of-call/cc),也就是加2

4.  continuation 本质上对应于栈, 是一种control context, 而environment是一种 data
    context.

5.  continuation 内部是静态作用域的, 这个性质有时会产生一些很难捕捉的bug.

6.  tail call(尾调用): 如果在函数p内调用了q, 而且q的返回值也是p的返回值, 那么 我们就说q是一个尾调用,
    尾调用是不会增加栈的, 因为它本质上就是一个goto语句.

## call/cc, let/cc

`call/cc` \[1\]是 `call-with-current-continuation` 的缩写. 它的基本形式是这样的:

``` scheme
(call/cc
  (lambda (k)                           ;k is the continuation
    (* 5 (k 4))))
;;; the result is 4
```

`let/cc` 可以看做是 `call/cc` 的一种简写:

``` scheme
(let/cc k                               ;k is the continuation
        body)
;;; identical
(call/cc (lambda (k)                    ;k is the continuation
           body))
```

`call/cc` 的参数是一个函数, 这个函数有一个参数, 这个参数会绑定到当前的 `continuation`, 具体到这个例子就是:
`k` 代表当前的continuation(也就是 `call/cc` 调用时的 `continuation`), 现在当你应用该
`continuation` 也就是使 用 `(k 4)` 时代码会立即从 `call/cc` 中返回, 并且返回值是 `4`, 注意它不会执行
前面的 (\* 5), 所以不要以为是返回 20. 下面在举几个例子:

``` scheme
(call/cc
  (lambda (k)
    (* 5 4)))                           ;return 20

(+ 2
   (call/cc
    (lambda (k)
      (* 5 (k 4)))))                    ;return 6

(define return #f)
(+ 1 (call/cc
      (lambda (cont)
        (set! return cont)
        1)))                            ;return 2
(return 22)                             ;return 23
```

再来看一个比较不好懂的例子

``` scheme
(((call/cc (lambda (k) k))
  (lambda (x) x)) "HEY!")
```

`(call/cc (lambda(k) k))` 返回当前的 `continuation`, 我假设该continuation为
`cont`, 那么 `cont` 必然是一个escape procedure,而且可以看做是如下定义的:

``` scheme
(lambda (val)
  ((val
    (lambda (x) x)) "HEY!"))
```

所以原来的表达式也就等价于 `((cont (lambda(x) x)) "HEY!")`,又因为cont是一个 escape
procedure,所以上面的表达式又等价于 `(cont (lambda(x) x))`,而该式很显 然返回 "HEY\!".

## 应用

1.  BREAK与RESUME, 通过BREAK来暂停, 通过RESUME来从暂停的位置启动.
    
    ``` scheme
    (define BREAK
      (lambda (message)
        (call/cc (lambda (k^)
                   (set! RESUME K^)
                   ((lambda^ (x) x) message)))))
    ```

2.  exceptions 比如racket的异常处理设施, with-handlers以及raise
    
    ``` example
    (with-handlers ([predicate-expr handler-expr] ...)
      body ...+)
    ```
    
    下面是一个示例代码:
    
    ``` scheme
    (define (always-fail n)
        (with-handlers ([even? (lambda (v) 'even)]
                        [positive? (lambda (v) 'positive)])
          (raise n)))
    ```
    
    这种异常处理设施可以使用continuation来创建, 实际上就是将 body 部分放入 let/cc中,
    调用raise实际就是调用continuation
    
    ``` scheme
    (define convert
      (lambda (exp)
        (match exp
               [`(with-handlers ([,preds ,handlers] ...)
                                ,body ...)
                `(let ([ret (let/cc k^
                                    ,@(map convert body))])
                   (cond
                    ,@(map (lambda (p h) `[(,p ret) (,h ret)]) preds handlers)
                    ))]
               [`(raise ,val) `(k^ ,val)]
               [`(lambda (,uvar ...) ,body ...)
                `(lambda (,@uvar)
                   ,@(map convert body))]
               [`(,f ,v ...)
                `(,(convert f)
                  ,@(map convert v))]
               [`(if ,test ,conseq ,alt)
                `(if ,(convert test) ,(convert conseq) ,(convert alt))]
               [x x])))
    ```
    
    上面这个函数就是将表达式中的with-handlers以及raise转换为continuation, 比 如下面的代码:
    
    ``` scheme
    (convert '(with-handlers ([even? (lambda (v) 'even)]
                              [positive? (lambda (v) 'positive)])
                             1
                             (raise 11)
                             2))
    ;; =>
    ;; '(let ((ret (let/cc k^ 1 (k^ 11) 2)))
    ;;    (cond
    ;;     ((even? ret) ((lambda (v) 'even) ret))
    ;;     ((positive? ret) ((lambda (v) 'positive) ret))))
    
    ```
    
    当然这个函数还有一些问题,但是我只是想演示一下异常处理设施的实现原理.

3.  generators

4.  coroutine

# CPS

[CPS](http://en.wikipedia.org/wiki/Continuation-passing_style)(continuation
passing style). 核心就是每一个函数都会带一个额外的参数
(continuation),前面说了continuation代表的是the rest of
computation, 因此这 个参数(continuation)代表了调用者需要对该函数的返回值进行的处理, 因此一个CPS
方式编写的函数最后都会使用函数的计算结果来调用你传递的那个continuation.

## 自动CPS转换

一个自动进行CPS转换的宏

``` scheme
(define-syntax CPS
  (syntax-rules ()
    [(_ (+ e1 e2))          ; other binary operators(- * / etc) are similar to +
     (lambda (k^)
       ((CPS e1)
        (lambda (v1)
          ((CPS e2)
           (lambda (v2)
             (k^ (+ v1 v2)))))))]
    [(_ (zero? exp))        ;other unary operators(number?, string? etc) are similar to zero?
     (lambda (k^)
       ((CPS exp)
        (lambda (val)
          (k^ (zero? val)))))]
    [(_ (if test conseq alt))
     (lambda (k^)
       ((CPS test)
        (lambda (v)
          (if v
              ((CPS conseq) k^)
              ((CPS alt) k^)))))]
    [(_ (lambda (x) body))
     (lambda (k^)
       (k^ (lambda (x)
             (lambda (k)
               ((CPS body) k)))))]
    [(_ (app arg))
     (lambda (k^)
       ((CPS app)
        (lambda (f-val)
          ((CPS arg)
           (lambda (arg-val)
             ((f-val arg-val) k^))))))]
    [(_ v) (lambda (k^ ) (k^ v))]          ; basic values(number, string etc) must stay at last
    ))
```

自动的CPS转换的要点

1.  首先任何表达式都要转换为 (lambda(k^) …) 的形式, 比如对一个常数表达式2, 你应该转换为 (lambda(k^) (k^
    2))的形式, 这样做这是因为正在转换的表达式可 能是一个大的表达式中的子表达式, 所以应该把转换后的表达式传递给一个
    continuation

2.  对于lambda表达式, 以(lambda(x) x)为例, 因为它实际和常量一样, 所以应该这样 转换:
    
    ``` scheme
    (lambda (k^)
      (k^ (lambda (x) x)))
    ```
    
    但是cps转换后lambda表达式是要接受两个参数, 也就是x与一个continuation, 所 以:
    
    ``` scheme
    (lambda (k^)
      (k^ (lambda (x dyn-k)
            ((lambda (k) (k x))
             dyn-k))))
    ;;; a simple veraion
    (lambda (k^)
      (k^ (lambda (x dyn-k)
            (dyn-k x))))
    ```
    
    注意上面的k^是lambda表达式定义时的continuation, 而dyn-k是lambda表达式应用
    时的continuation,因为应用时continuation可以不同所以它也就是动态的.

## CPS的应用

如果将一个解释器转换为CPS形式, 那么就可以很容易的实现像 scheme中call/cc,
let/cc这样可以获得当前continuation的结构, 因为解释器的continuation代表的解
释器接下来要进行的计算, 而解释器是用来模拟用户程序的, 所以实际上这个
continuation也可以看作是用户程序接下来要完成的计算,也就是用户程序当前的
continuation.

# Footnotes

1.  [wikipedia:
    Call-with-current-continuation](https://www.wikiwand.com/en/Call-with-current-continuation)
