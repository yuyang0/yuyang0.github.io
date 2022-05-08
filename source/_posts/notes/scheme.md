---
title: Scheme笔记
date: 2014-02-18 11:54:17
tags:
- scheme
- lisp
- lang
categories:
- notes
---
# scheme笔记

## 几个概念 <span class="tag" data-tag-name="concept"><span class="smallcaps">concept</span></span>

### identifier

标识符,可以看作是一个唯一的名字,可以用来引用变量,函数等等

### variable

变量

### atom

  - a string of characters
  - a string of digits

<!-- end list -->

``` scheme
(define atom?
  (lambda (s)
    (and (not (pair? s))
         (not (null? s)))))
```

### list

a collection of atoms enclosed by parentheses. example:

``` example
'()
'(a)
'(a b c)
'((a b) c d)
```

语法描述:

``` example
list -> '()
      | (sexp . list)
```

### S-expression

  - all lists
  - all atoms

<!-- end list -->

``` example
sexp -> list
      | atom
```

## Naming Conventions

1.  谓词后加? , 但是常用的数字比较=, \<, \>, \<=, \>=后面不需要?
2.  类型测试, pair? , atom?
3.  字符操作(char-xxx), 字符串操作(string-xxx), 向量操作(vector-xxx)
4.  类型转换(type1-\>type2)
5.  但函数有副作用时,应该以 \! 结尾, 比如 `set!`

## core syntactic forms <span class="tag" data-tag-name="core"><span class="smallcaps">core</span></span> <span class="tag" data-tag-name="syntactic"><span class="smallcaps">syntactic</span></span> <span class="tag" data-tag-name="form"><span class="smallcaps">form</span></span>

1.  top-level `define` forms 实际就是创建新的绑定,可以绑定 `list`, `lambda procedure`
    这是几个示例代 码:
    
    ``` scheme
    (define double-any
      (lambda (f x)
        (f x x)))
    ;;; identical
    (define (double-any f x)
      (f x x))
    
    (define xyz '(x y z))
    ```

2.  constants BNF:
    
    ``` example
    <constant> -> <boolean>
                | <number>
                | <character>
                | <string>
    ```

3.  variables

4.  procedure applications `(procedure arg1 ... argn)` 求值规则:
    
    1.  Find the value of `procedure`.
    2.  Find the value of `arg1`.
    3.  Find the value of `argn`.
    4.  Apply the value of `procedure` to the values of `arg1` … `argn`.

5.  `quote` expressions (')
    
    1.  quoting an `identifier` tells Scheme to treat the identifier as
        a `symbol` rather than as a `variable`. 也就是说scheme解释器不会去当前的
        环境中寻找与该标识符绑定的值, 而是直接将该标识符当作symbol也就是数据处理.
    2.  quoting a list tells scheme to treat the list as data, rather
        than as a procedure application

6.  `lambda` expressions `(lambda (x) (+ x x)) ==> #<procedure>`
    
    一般形式有这三种:
    
      - `(lambda (var1 var2 ...) exp1 exp2 ...)`: var1 var2 …会依次赋值
      - `(lambda (var1 var2 . var) exp1 exp2)`: var1 var2会依次赋值, 余下的参数
        会组成一个列表赋给var
      - `(lambda var exp1 exp2)`: 将所有的实参作为一个list赋给var, 注意var没有括号

7.  `if` expressions: `(if test-expr then-expr else-expr)` 只有
    `test-expr` 为 `#f` 时才会运行 `else-expr`, 所以你一般要使用 `null?` `eq?` 这样的函数
    来测试

8.  `set!` expressions(Assignment): Assignments do not create new
    bindings, as with `let` or `lambda`, but rather change the values of
    existing bindings.也就是说赋值不会像let, lambda那样产生新的绑定,而是会改变已存在绑
    定的值,如果你给set\!指定的符号不存在,它会报错(set\!: cannot set undefined
    variable)

其它的都是一些扩展,也就是可以通过define-syntax定义出来的,比如let,and,or,not等
等,只有以上的部分才需要解释器直接实现的

``` scheme
(define-syntax let
  (syntax-rules ()
    [(_ ((x v) ...) e1 e2 ...)
     ((lambda (x ...) e1 e2 ...) v ...)]))

(define-syntax and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (if e1 (and e2 e3 ...) #f)]))
```

实际上define-syntax实际就是进行模式匹配,像cond,如果匹配那么就进行替换,比 如(and 1)就匹配((\_ e)
e)所以就被替换为1, <sub>就是and的占位符</sub>, 而 `pat ...` 代表 0个或者多个表达式, 比如
`(x v) ...` 代表有0个或多个 `(x v)` 这样的表达式

## let <span class="tag" data-tag-name="let"><span class="smallcaps">let</span></span>

``` scheme
;;; form 1
(let ((a exp1)
      (b exp2))
  (body1)
  (body2))
;;; form2
(let f ([a exp1]
        [b exp2])
  (body1)
  (body2))
```

由于 `scheme` 对待中括号与对待小括号是一样的,所以为了可读性, `let` 一般可以用如下代 码:

``` scheme
(let ([a exp1]
      [b exp2])
  (body1)
  (body2))
```

form2主要是为了递归, 它使得在函数体中可以引用函数名, 比如下面的代码

``` scheme
(let fac ([n 10])
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))
```

  - let: 如上所见,实际就是一个扩展语法,a,b只在body中可见, 所以你在exp2中不能使 用a

  - let\*: exp2中可以引用a, 也就是a可以用来定义b, 可以用嵌套的 `let` 来定义 `let*`
    
    ``` scheme
    (define-syntax let*
      (syntax-rules ()
        [(_ () e1 e2 ...)
         (let () e1 e2 ...)]
        [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
         (let ((x1 v1))
           (let* ((x2 v2) ...) e1 e2 ...))]))
    ```

  - letrec: 主要用来解决定义递归函数时,函数名在函数体中不可见的问题,比如
    
    ``` scheme
    (let ([sum (lambda (lst)
                    (if (null? lst) 0
                        (+ (car lst) (sum (cdr lst)))))])
      (sum '(1 2 3 4 5)))
    ```
    
    如果运行上面的代码,那么你会 `sum undefined` 的错误, 原因是 `sum` 在后面的函数 体中不可见, 所以你使用
    `(sum (cdr lst))` 就会出错,当然你可以使用这种方法
    
    ``` scheme
    (let ([sum (lambda (sum lst)
                    (if (null? lst) 0
                        (+ (car lst) (sum sum (cdr lst)))))])
      (sum sum '(1 2 3 4 5)))
    ```
    
    但这种方法不够自然,而且比较丑陋,不符合scheme中定义递归函数的一般模式,所以 就引入了 `letrec`.
    
    ``` scheme
    (letrec ([sum (lambda (ls)
                    (if (null? ls)
                        0
                        (+ (car ls) (sum (cdr ls)))))])
      (sum '(1 2 3 4 5)))
    ;; the result is 15
    ```

  - letrec\*: 和 `letrec` 类似, 只是后面的绑定可以引用前面已经绑定的变量

  - let-values: 绑定多个变量 syntax:
    
      - (let-values ((formals expr) …) body1 body2 …)
      - (let\*-values ((formals expr) …) body1 body2 …)
    
    <!-- end list -->
    
    ``` scheme
    (let-values ([(a b) (values 1 2)] [c (values 1 2 3)])
      (list a b c)) ; the result is (1 2 (1 2 3))
    
    (let*-values ([(a b) (values 1 2)] [(a b) (values b a)])
      (list a b)) ; the result is (2 1)
    ```

## control flow

### if <span class="tag" data-tag-name="if"><span class="smallcaps">if</span></span>

当要写多个表达式时应该加入 `begin` , 注意 `begin` 会依次执行它所包含的表达式,并 返回最后一个表达式的值

``` scheme
(if (test-exp)
    (begin
      expression1
      expression2)
    expression3)
```

如果 `test-exp` 为true, 依次执行 `expression1`, `expression2` 那么返回
`expression2` 的值, 之所以需要 `begin` 是由 `if` 的语法决定的, `if` 的语 法如下:

``` scheme
(if (test-exp)
    (true-exp)
    (false-exp))
```

所以如果你在 `true-exp` 的位置放入多条表达式,那么这些表达式的第二条会 被当作 `false-exp` ,而且因为 `if`
后的表达式的条数超出 `2` 条而报错, 所以 你需要把多条表达式括起来,但是你不能直接加一个括号,比如

``` example
((true-exp1)(true-exp2))
```

这样之所以不行是因为scheme会将 (true-exp1) 当作procedure求值,这显然不 对,
所以scheme使用begin(更准确的说begin是为了引入side effect), 也就变成 了:

``` example
(begin
  (true-exp1)
  (true-exp2))
```

这就是if使用begin的原因了, 注意在 let, lambda, define,cond的body中都不需
要begin,因为它们都没有if这种特殊的状况.

### cond <span class="tag" data-tag-name="cond"><span class="smallcaps">cond</span></span>

内部相当于有个隐含的begin,所以可以直接写多个表达式

``` scheme
(cond [(test-exp) (exp1) (exp2)]
      [else expression3])
```

如果test-exp为true, 那么返回expression2的值 注意一个特殊的形式: =\>

``` scheme
(cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f))                       ;; ==>  2
```

会将 test-exp的值传递给 =\>后面的函数, =\>后面必须是一个带一个参数的函数

### when unless <span class="tag" data-tag-name="when"><span class="smallcaps">when</span></span> <span class="tag" data-tag-name="unless"><span class="smallcaps">unless</span></span>

  - (when (test-exp) exp1 exp2) : test-exp为真就执行exp1, exp2
    
    ``` scheme
    (define-syntax when
      (syntax-rules ()
        [(_ e0 e1 e2 ...) (if e0 (begin e1 e2 ...))]))
    ```

  - (unless (test-exp) exp1 exp2) : 只有当 `test-exp` 为\#f时才会运行body中的 表达式

### case <span class="tag" data-tag-name="case"><span class="smallcaps">case</span></span>

类似于C语言的switch

``` scheme
(let ((x 4) (y 5))
  (case (+ x y)
    ((1 3 5 7 9) 'odd)
    ((0 2 4 6 8) 'even)
    (else 'out-of-range))) ;; ===> odd
```

### do <span class="tag" data-tag-name="do"><span class="smallcaps">do</span></span>

`syntax: (do ((var init update) ...) (test result ...) expr ...)`

循环(do ((var init update) …) (test res …) exp …) var的初始值是init,接
着每一次迭代都绑定到update, (test res..)如果为true,那么就终止循环

``` scheme
(define divisors
  (lambda (n)
    (do ((i 2 (+ i 1))
         (ls '()
             (if (integer? (/ n i))
                 (cons i ls)
                 ls)))
        ((>= i n) ls))))
```

### map

(map procedure list1 list2 …) 会返回一个 `list`

``` scheme
(map (lambda (x y) (* x y))
     '(1 2 3 4)
     '(8 7 6 5))      ; return (8 14 18 20)
```

`map` 与 `for-each` 可以代替许多循环的工作, 而且逻辑上比循环更清晰.

### for-each

和 `map` 类似但是不返回一个list作为结果, 也就是说 `for-each` 是用来产生side effect.

### apply

(apply procedure obj … list)

  - `(apply + '(4 5)) ===> 9`
  - `(apply min 5 1 3 '(6 8 3 2 5))` 结果是1

### multiple values

  - (values obj …) :返回多个值,注意它的返回值可以用 `let-values` 来绑定,不能用 `define`
    
    ``` scheme
    (let-values ([(a b) (values 1 2)])
      (+ a b))
    ```

  - (call-with-values producer consumer): producer产生多个值, 然后将这些值传
    递给consumer.注意producer必须可以不带参数的方式来调用
    
    ``` scheme
    (call-with-values
      (lambda () (values 'bond 'james))
      (lambda (x y) (cons y x)))  ; the result is (james . bond)
    ```

## 内置的函数

### predication

1.  \= : 只用来比较整数, 不要用来比较浮点数
2.  eq? : 可以类似的看作是指针比较, 即便是内容相同,但是如果是两个不同对象,它 就返回 `#f`, 比如 `(eq? (cons
    'a 'b) (cons 'a 'b))` 虽然是同样的list, 可 是它在内存中的位置不同,所以为 \#f.
    有以下几种情况是相等的.
      - \#t, \#f, 两个identifier的值如果都是\#t或者\#f,那么它们相等
    
      - 相同的符号(symbol), 比如
        
        ``` scheme
        (define sym1 'hello)
        (define sym2 'hello)
        (eq? sym1 sym2)
        ```
    
      - '()
3.  eqv? : 和 `eq?` 很类似, 只是它在一些 `eq?` 没有定义的地方也可以使用
4.  equal? : 只要内容相同,它就会返回 \#t,可以认为它的检查比 `eq?` 宽松
5.  boolean? : 等价于 `(lambda (x) (or (eq? x #t) (eq? x #f)))`
6.  null? : 只作用于list, only `(null? '())` return `#t`.
7.  pair? :
8.  number? :
9.  complex? :
10. rational? :
11. real? :
12. integer? :
13. char? :
14. string? :
15. vector? :
16. symbol? :
17. procedure? :
18. bytevector? :
19. hashtable? :

### list procedure

`list` 的语法定义:

``` example
List ::= '()
     ::= (Sexp . List)
```

1.  cons: (cons 'a 'b) ==\> (a . b),只能带两个参数

2.  cons\* : (cons\* 'a 'b 'c) ==\> (a b . c) (cons\* 'a 'b '(c)) ==\>
    (a b c)

3.  car: the firsts element of pair (**only for non-empty list** )

4.  cdr: Only for non-empty list, the cdr of any non-empty list is also
    a list.

5.  set-car\! :有副作用, 会原地改变pair的值

6.  set-cdr\! :

7.  car, caar, caaar…etc: 连续执行n次(a的次数)car (caar '((5)) ) ===\> 5

8.  cdr cddr cdddr …etc : 连续执行n次(d的个数)cdr

9.  cadar : (car (cdr (car lst))) 记住按照顺序从左到右,最右边的先对list起作用

10. list: 创建一个list eg: (list 1 2 3 4) ===\> '(1 2 3 4)

11. length: list的长度

12. append: 将一个list添加另一个list的后边,eg: (append '(1 2) '(3 4)) ===\> '(1 2 3
    4)

13. reverse: 将list倒转

14. (list-ref list n): 第n个元素

15. (list-tail list n) : 倒数第n个元素

16. memq memv member memp 分别用eq? eqv? equal? 指定的procedure 来测试一个元
    素是否属于list, 如果属于那么就返回包括该元素以及该元素后面的元素组成的 list, `(memq 'a '(b
    c a d e)) --> (a d e)`
    
    ``` scheme
    (define memq
      (lambda (x ls)
        (cond
          ((null? ls) #f)
          ((eq? (car ls) x) ls)
          (else (memq x (cdr ls))))))
    ```

17. remq (obj list) remv remove remp 删除list中的所有obj

18. assq assv assoc assp 可以看做是关联数组((key1 . val1) (key2 . val2) …)
    
    ``` scheme
    (define assq
      (lambda (x ls)
        (cond
          ((null? ls) #f)
          ((eq? (caar ls) x) (car ls))
          (else (assq x (cdr ls))))))
    ```
    
    返回第一个匹配的pair

19. (filter procedure list): return a list of the elements of list for
    which procedure returns true

20. (partition procedure list): 返回两个list,第一个list包含所以使 `procedure`
    为\#f的元素,第二个包含所有使 `procedure` 为 `#f` 的元素, `partition`
    的返回值可以使用 `let-values` 来绑定或者使用 `call-with-values`
    来调用其它函数

21. (find procedure list) : 返回第一个使procedure为\#t的元素

22. (map f list1 list2 …)

23. (foldl f init list1 list2 …): **racket 的内置版本有bug**

24. (foldr f init list1 list2 …):
    
    ``` scheme
    (define foldl
      (lambda (f x ls)
        (cond
         [(null? ls) x]
         [else
          (foldl f (f x (car ls)) (cdr ls))])))
    ;; (foldl op init '(1 2 3 4))
    ;; we can think foldl's behavior like this: (init op 1 op 2 op 3 op 4)
    ;; op has left associativity
    (define foldr
      (lambda (f x ls)
        (cond
         [(null? ls) x]
         [else
          (f (car ls) (foldr f x (cdr ls)))])))
    ;; (foldr op init '(1 2 3 4 5))
    ;; we can think foldl's behavior like this: (1 op 2 op 3 op 4 op init)
    ;; op has right associativity
    
    ```

### number procedure

1.  zero? :
2.  positive? :
3.  negative? :
4.  even? :
5.  odd? : 奇数
6.  数学函数: max, min, floor, ceiling(向+无穷取整),
    truncate(向0取整),round(最接近的整数),
    abs,gcd,expt(指数),三角系列函数(san,cos….)

### char

字符使用 `#\x` 来表示,比如 `a` 就是 `#\a`,

  - (char=? char1 char2 char3 …)

  - (char\<? char1 char2 char3 …)

  - (char\>? char1 char2 char3 …)

  - (char\<=? char1 char2 char3 …)

  - (char\>=? char1 char2 char3 …)

  - (char-ci=? char1 char2 char3 …) 大小写敏感

  - (char-ci\<? char1 char2 char3 …)

  - (char-ci\>? char1 char2 char3 …)

  - (char-ci\<=? char1 char2 char3 …)

  - (char-ci\>=? char1 char2 char3 …)

  - (char-upcase char) :大写,相对应的还有个char-downcase

  - (char-\>integer char)

  - (integer-\>char n)

### string

双引号内的是字符串

  - (string=? char1 char2 char3 …)
  - (string\<? char1 char2 char3 …)
  - (string\>? char1 char2 char3 …)
  - (string\<=? char1 char2 char3 …)
  - (string\>=? char1 char2 char3 …)
  - (string-ci=? char1 char2 char3 …) 大小写敏感
  - (string-ci\<? char1 char2 char3 …)
  - (string-ci\>? char1 char2 char3 …)
  - (string-ci\<=? char1 char2 char3 …)
  - (string-ci\>=? char1 char2 char3 …)
  - (string char …): 构建个包含指定的字符的字符串
  - (make-string n), (make-string n char)
  - (string-append string …)
  - (substring string start end)
  - (string-upcase string)
  - (string-\>list): 把string转换成包含char的list
  - (char-\>list lst)

### vector procedure

list访问时需要遍历,不够高效, vector可以像数组一样O(1)时间内访问

1.  (vector obj …)
2.  make-vector: (make-vector 5 'a) ===\> \#(a a a a a)
3.  vector-length: vector的长度
4.  (vector-ref vec n)
5.  (vector-set\! vec n obj)
6.  (vector-fill vec obj): 所有的元素都替换为obj
7.  list-\>vector :
8.  vector-\>list :

### symbol procedure

每一个symbol在解释器内部都是指向同一个对象,所以用eq?来测试相同的symbol会返 回\#t, 这也使得比较操作很高效

1.  symbol=? :symbol是否相等,也可以用eq?来比较
2.  symbol-\>string:
3.  string-\>symbol:

### Hash Table

  - (make-eq-hashtable) : 使用eq?来比较两个key, 返回一个hashtable

  - (make-eq-hashtable size)

  - (make-eqv-hashtable) : 使用eqv?来比较两个key

  - (make-eqv-hashtable size)

  - (make-hashtable hash equiv?) : hash指定hash函数, equiv?指定比较两个key的 函数

  - (make-hashtable hash equiv? size)

  - (hashtable-set\! hashtable key obj) :

  - (hashtable-ref hashtable key default) :

  - (hashtable-delete\! hashtable key) :

  - (hashtable-size hashtable) : hashtable的大小

  - (hashtable-contains? hashtable key) :测试是否包含指定的key

## macros

定义一个宏实际上就是把一个keyword与一个 macro transformer 绑定, 一个macro
transformer一般就是一个带有一个参数的函数, 而macro transformer的输入是一个原
始代码的syntax object, 输出则是包含转换后代码的 syntax object. `syntax-rules`
返回的就是一个macro transform.所以你可以抛开 `syntax-rules` 而这样写一个宏:

``` scheme
(define-syntax self-as-string
  (lambda (stx)
    (datum->syntax stx
                   (format "~s" (syntax->datum stx)))))
```

### define-syntax

和define类似

``` scheme
(define-syntax let*
  (syntax-rules ()
    [(_ () b1 b2 ...) (let () b1 b2 ...)]
    [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
     (let ([i1 e1])
       (let* ([i2 e2] ...) b1 b2 ...))]))
```

### let-syntax与letrec-syntax

``` scheme
(let ([f (lambda (x) (+ x 1))])
  (let-syntax ([f (syntax-rules ()
                    [(_ x) x])]
               [g (syntax-rules ()
                    [(_ x) (f x)])])
    (list (f 1) (g 1)))) ;; (1 2)

(let ([f (lambda (x) (+ x 1))])
  (letrec-syntax ([f (syntax-rules ()
                       [(_ x) x])]
                  [g (syntax-rules ()
                       [(_ x) (f x)])])
    (list (f 1) (g 1)))) ;; (1 1)
```

### syntax-rules

基本语法: `(syntax-rules (literal ...) (pattern template) ...)`

返回一个transformer, 实际就是一个 `procedure`, 这个 `procedure` 接受一个 syntax
object,然后返回一个syntax object.

  - literal: 一些关键字, 出现在pattern会原样匹配
  - pattern: 用于匹配输入的表达式的模式
  - template: 输出,记住template是原样输出,这是和syntax-case的最大区别.
  - 下划线'\_': 可以匹配任何结构,一般用来代表宏名
  - … : 代表前面的部分重复0次或者多次

一个例子,这个例子实现一个类 `if` 的条件判断语句,他的形式是 `(my-if cond then
exp1 else exp2)` 这看起来更可读性更好:

``` scheme
(define-syntax my-if
  (syntax-rules (then else)
    [(_ condition then true-exp else false-exp)
     (if condition true-exp false-exp)]))
```

`then, else` 是关键字,所以它会原样的匹配输入

### syntax object

`(syntax template)` 的缩写为 `#'template`, syntax和quote很类似,只是它会将
template中的pattern variable替换掉, 并且会绑定上下文信息

1.  syntax: 创建一个字面的 syntax object,比如 `(syntax '(+ 1 x))`, 简写 `#'(+ 1
    x)`.
2.  syntax-\>datum: 将一个syntax object转换为它原来的内容.
3.  identifier? : 测试一个syntax object 是不是标识符
4.  syntax-e : 只解包一层.

### syntax-case

比 `syntax-rules` 更具一般性,而且更通用, 使用syntax-case的一般形式是:

``` example
(define-syntax macro-name
   (lambda (x)
     (syntax-case x (other keywords go here if any)
       [
         (macro-name macro-arg1 macro-arg2)
         ;;Expansion of macro (one or multiple forms)
         ;;(syntax is a reserved word)
         (syntax (expansion of macro goes here))
       ]  ...
 )))
```

解释一下上面的代码: 整个lambda定义的就是一个macro transformer, 它的唯一参数 x
实际就是一个包含了原始代码的syntax object. 通过syntax-case
来匹配x,然后返 回一个包含了转换后代码的syntax object

``` scheme
(define-syntax my-if
  (lambda (x)
    (syntax-case x (then else)
      [(_ condition then true-exp else false-exp)
       (syntax (if condition true-exp false-exp))])))
;;Define a new macro
(define-syntax swap!
  (lambda (x)
    ;;we don't have any keywords this time
    (syntax-case x ()
      [(swap! a b)
       (syntax
        (let ((c a))
          (set! a b)
          (set! b c)))]
      )))

(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       #'(lambda (x)
           (syntax-case x (i ...)
             [(_ . pattern) #'template] ...))])))
```

### with-syntax

``` scheme
(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((p e) ...) b1 b2 ...)
       #'(syntax-case (list e ...) ()
           [(p ...) (let () b1 b2 ...)])])))
```

## input and output

### port

scheme的输入输出用到一个叫做port的对象,port也是first class object, port有很
多种类,比如文件,字符缓冲区, tcp网络连接, 管道等等

1.  文件:
      - (open-output-file fname) : 返回一个port,用于将内容写入到文件
      - (open-input-file fname) : 返回一个port用于读取指定文件的内容
      - (close-output-port port)
      - (close-input-port port)
    还有一种常用方式(更简单, 会自动关闭port):
      - (call-with-input-file filename proc) : 打开filename并将得到的port传递
        给proc, 并且调用完成时会关闭port
    
      - (call-with-output-file filename proc) : 打开filename并将得到的port传递
        给proc, 并且调用完成时会关闭port
        
        ``` scheme
        (call-with-output-file "data"
          #:exists 'truncate
          (lambda (out)
            (display "hello" out)))
        
        (call-with-input-file "data"
          (lambda (in)
            (read-line in)))
        ```
2.  字符缓冲区
      - (open-output-string) :
      - (open-input-string "xxx"):

<!-- end list -->

1.  default port
    
    默认解释器会打开2个port, 一个输入,一个输出,分别会绑定到shell的输入输出,很 多实现还会打开一个error
    port,用于IO的scheme procedure都可以带一个可选的参 数port, 如果不指定port,
    那么就使用默认的port, 默认的port有这两个函数获得
    
      - (current-input-port)
      - (current-output-port)
    
    如果要改变默认的port,可以使用下面的两个函数:
    
      - (with-input-from-file filename thunk) : 将默认的 input port重新绑定到文
        件,这可以实现重定向
      - (with-output-from-file filename thunk) : 将默认的 output port重新绑定到
        文件,这可以实现重定向

### 输入输出

  - 输入:
    1.  read: 它会自动将读入的内容转换为scheme内置的数据结构,实际是一个递归下 降的parser
    2.  (read-char), (read-char input-port): next character
  - 输出:
    1.  print
    2.  write
    3.  display
    4.  printf: 可以指定一个字符串来格式化,用的较多
    5.  fprintf: 写入文件,多一个port参数

### code example

``` scheme
(call-with-input-file "myfile.ss"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x)
          '()
          (cons x (f (read p)))))))

(define read-word
  (lambda (p)
    (list->string
      (let f ()
        (let ((c (peek-char p)))
          (cond
            ((eof-object? c) '())
            ((char-alphabetic? c)
             (read-char p)
             (cons c (f)))
            (else '())))))))

```

## 递归思想

递归的思想要从逻辑上理解，在设计一个递归函数时，一开始就要明确该函数在逻辑上
的作用(不要一开始就陷入编码细节中)，然后分清该函数在逻辑上与子问题的联系，并
以此为 依据来进行函数体的设计, 特别需要注意检查结束条件,比如加法(0), 乘法 (1), list('())就是一些结束条件.

递归函数的设计要点：

1.  要明确检查终止条件（null？ zero？）
2.  要明确与子问题的关系，弄清分类的情况（cond）

## continuation

scheme中获得当前continuation的构造是call/cc, let/cc

``` scheme
(let/cc k                               ;k is the continuation
        body)
;;; identical
(call/cc (lambda (k)                    ;k is the continuation
           body))
```

## 停机问题

``` scheme numberLines
(define last-try
  (lambda ( x )
    (and ( will-stop ? last-try)
         ( eternity x ))))
```

上述代码中:eternity会永远运行,比如:

``` scheme
(define eternity
  (lambda (x)
    (eternity x)))
```

假设存在停机函数 `will-stop?`, `will-stop?` 可以测试出一个函数是否会停机.那么:

1.  假设 `last-try` 会停机, 那么 `(will-stop? last-try)` 返回 \#t, 因此也就会运行
    `(eternity x)`, 前面说了, eternity会永远运行, 所以 `last-try` 不会停机.
2.  假设 `last-try` 不停机, 那么 `(will-stop? last-try)` 返回 \#f, 因此也就不会运 行
    `(eternity x)`, 那么 `last-try` 很显然就会返回. 所以 `last-try` 会停机

因此这就是个悖论.因此停机函数 will-stop?不存在

## some example code(the little schemer)

# racket

## struct(新的数据类型) <span class="tag" data-tag-name="struct"><span class="smallcaps">struct</span></span>

语法形式: `(struct struct-id (field-id ...))`

  - struct-id : 是一个constructor, 可以用来构建一个该数据类型的实例
  - struct-id? :一个predication,测试是否是该数据类型的实例
  - struct-id-field-id: 从实例中获取 `field-id` 属性的值

下面来看个例子:

``` scheme
(struct posn (x y))                     ;定义一个posn类型
(define pos1 (posn 1 2))                ;struct-id: 构建一个posn实例
(posn? pos1)                            ;struct-id?: 是否为一个posn对象实例
(posn-x pos1)                           ;struct-id-field-id: 获得x属性
```

### 复制更新

根据已有对象更新其中的特定域然后返回新对象

语法形式: `(struct-copy struct-id struct-expr [field-id expr] ...)`

``` example
Examples:

> (define p1 (posn 1 2))
> (define p2 (struct-copy posn p1 [x 3]))
> (list (posn-x p2) (posn-y p2))

'(3 2)
> (list (posn-x p1) (posn-x p2))

'(1 3)
```

### subtypes(类似于继承) <span class="tag" data-tag-name="subtypes"><span class="smallcaps">subtypes</span></span>

语法形式: `(struct struct-id super-id (field-id ...))`

``` scheme
(struct posn (x y))
(struct pos3d posn (z))
```

那么pos3d就有 `(x y z)` 三个属性

## match(模式匹配) <span class="tag" data-tag-name="match"><span class="smallcaps">match</span></span>

正则表达式只能用来匹配字符串,而 `match` 可以用来匹配任何的 `scheme value`, 它的语法形式如下:

``` example
(match target-expr
  [pattern expr ...+] ...)
```

将 `target-expr` 与 pattern匹配, 如果匹配成功就执行后面的expr, 对pattern的 语法要做以下说明:

  - … 或 <span class="underline">\_</span> : 代表0次或者多次

  - ..k 或 \_<sub>k</sub> : 代表至少 `k` 此 下面是一个将let转换为等价的lambda形式的例子
    
    ``` scheme
    (match '(let ([a 1]
                  [b 2])
              (set! a 11)
              (- a b))
           [`(let ([,var* ,val*] ...) ,body* ...)
            `((lambda (,@var*)
                ,@body*) ,@val*)])
    ```
    
    注意因为…的作用,上面的var\*, val\*, body\*都是列表, 所以在后面需要使用 ,@来 分解.

  - literal: 字面值直接用 `equal?` 测试是否相等
    
    ``` scheme
    (match 2
           [1 'one]
           [2 'two]
           [3 'three])
    (match #f
           [#t 'yes]
           [#f 'no])
    ```

  - (list lvp …): 会绑定对应的标识符,注意几个特殊符号,vector和list类似
    
    ``` scheme
    (match '(1 + 2)
    [(list a '+ b) (+ a b)])         ;return 3
    
    (match '(1 2 3)
    [(list 1 a ...) a])                 ;return '(2 3)
    
    (match '(1 2 3 4)
    [(list 1 a ..3) a]
    [_ 'else])                          ;return '(2 3 4)
    ```
    
    上例中 `a b` 就被绑定为 1, 2

  - (struct-id pat …)或者(struct struct-id (pat …)):匹配一个实例,并且绑定 一些变量
    
    ``` scheme
    (define-struct tree (val left right))
    (match (make-tree 0 (make-tree 1 #f #f) #f)
           [(tree a (tree b  _ _) _) (list a b)])       ;'(0 1)
    ```

  - (struct struct-id \_) :匹配任何 struct-id的实例

  - (? expr pat …): expr是一个predication, 只有它返回true的时候,才会匹配后 面的pat
    
    ``` scheme
    (match '(1 3 5)
           [(list (? odd?) ...) 'yes])
    ```
    
    它的工作原理是这样,以下面的例子为例:
    
    ``` scheme
    (match '((1 2) 3 6)
           [`(,(? pred? `(,a1 ,a2)) ,b ,c) `(,a1 ,b ,c)])
    ```
    
    如果上面的例子匹配, 那么b, c应该分别绑定3, 6, 那么前面的 ``(? pred? `(,a1
        ,a2))`` 应该匹配(1 2), 所以先将(1 2) 传递给pred?, 如果pred? 返回\#t, 那么接 着用(1 2)
    去匹配后面的\`(,a1 ,a2). 也就是说pred? 只会检查(? pred? …) 匹配 的那一部分.

  - (quasiquote qp): unqote或unquote-splicing的部分会绑定为变量,其它部分会原样匹配
    
    ``` scheme
    (match '(1 + 2)
    [`(,a + ,b) (+ a b)])
    ```
    
    a b都是unquote指定的部分,所以绑定为变量, 其它部分比如 + 就原样匹配, 可以和 (? expr pat …)结合使用:
    
    ``` scheme
    (match '(+ a "hello")
           [`(+ a ,(? number? x)) x]
           [`(+ a ,(? string? x)) x])
    ```
    
    关于quasiquote的一些说明:
    
      - quasiquote(\`): 和quote(')类似, 只是表达式中的unquote会求值在返回,如果没
        有unquote,那么它的行为就和 `quote` 一样,来看几个例子: `(cons a b)` 等价于
        `` `(,a ,b) ``
    
      - unquote(,): 指定的部分会先求职,在插入list中
        
        ``` scheme
        (quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
        ;;; equivalent
        `(1 2 ,(+ 1 2) ,(+ 5 1))                ;'(1 2 3 6)
        ```
    
      - unquote-splicing(,@): 和unquote的行为类似, 只是它所指定的表达式求值后必
        须返回list,这个list中的元素会拆开然后插入原list中
        
        ``` scheme
        `(1 2 ,@(list (+ 1 2) (2 2)) 5)         ;'(1 2 3 4 5)
        ```

## module

module的基本语法是:

``` example
(module name-id initial-module-path
  decl ...)
```

例子:

``` scheme
(module cake racket
        (provide print-cake)

        (define (print-cake n)
          (show "   ~a   " n #\.)
          (show " .-~a-. " n #\|)
          (show " | ~a | " n #\space)
          (show "---~a---" n #\-))

        (define (show fmt n ch)
          (printf fmt (make-string n ch))
          (newline)))
```

  - name-id: module的名字,上例是 `cake`
  - initial-module-path: 初始化要导入的module, 上例是 `racket`
  - provide : 可选的,也就是规定哪些东西是可以导出的,上例中 `print-cake` 会导 出,但是 `show` 是模块私有,
    如果不提供 `provide` 那么所有的属性都是私有的, 所以一个模块必须指定 `provide` 才对导入者有意义

声明一个模块并不会直接对模块的 `body` 部分求值,只有当使用 `require` 明确导 入该模块时才会求值

### submodule

一个文件只能包含一个顶层模块,这个模块可以通过 `module` 指定,也可以通 过 `lang lang-name` 来间接的指定,
那么在这个顶层模块中又可以定义子模块,比 如如下代码:

``` scheme
#lang racket
(module zoo racket
        (provide tiger)
        (define tiger "Tony"))

(require 'zoo)

tiger
```

上面的代码(假设文件名是park.rtk) `#lang racket` 指定了顶层模块, 而 `zoo` 是 一个子模块,
在顶层模块中可以直接通过 `(require 'zoo)` 来包含子模块, 如果是 在该文件以外,
你需要导入该文件的模块, 使用 `(require "park.rtk")` 只会导入 顶层模块, 使用submode语法比如
`(require (submod "park.rtk" zoo))` 就可以导 入子模块了

1.  module\* :module\*:
    
    ``` example
    (module* name-id initial-module-path-or-#f
    decl ...)
    ```
    
    使用 `module` 声明的子模块可以可以被父模块导入, 但是子模块不能导入父模块, 而恰恰相反,使用 `module*`
    声明的模块可以导入父模块, 可是父模块不能导入该子 模块, 如果指定 `#f` 作为默认导入模块,
    那么父模块中的所有绑定在子模块中都可 见.
    
    ``` scheme
    #lang racket
    
    (define (print-cake n)
      (show "   ~a   " n #\.)
      (show " .-~a-. " n #\|)
      (show " | ~a | " n #\space)
      (show "---~a---" n #\-))
    
    (define (show fmt n ch)
      (printf fmt (make-string n ch))
      (newline))
    
    (module* main #f
             (print-cake 10))
    ```
    
    在上面的 `main` 这个submodule中, 顶层模块的所有绑定都可见, 注意当一个
    submodule的名字为main时,有一个特殊的地方,也就是说当前的文件作为执行文件时
    (racket file-name.rtk),即便你没有使用 `(require 'main)` 语句, 这个子模块仍
    然会运行,和python的=ifmain= 很类似,所以这个main模块可以写一些本模块的测试 代码

2.  module+ :module+:
    
    ``` example
    (module+ name-id
    decl ...)
    ```
    
    等价于
    
    ``` example
    (module* name-id #f
      decl ...)
    ```
    
    module+一般用来写 `test` 模块, 多个test模块会合并为一个test模块, 使用raco test
    filename.rkt来运行测试代码

### lang

racket的源文件一般需要使用 `#lang lang-name` 这样的方式来指定语言,这个实际是一 个module的简写方式,比如
`#lang racket` 等价于:

``` example
(module name racket
  decl ...)
```

所以 `#lang racket` 的意思就是定义一个module, 该模块的名字一般继承自文件名, 然后将racket作为初始模块导入.

### require

1.  如果是文件那么应该使用这样的语法:
    
    ``` scheme
    (require "aa.rkt")
    (require "../aa.rkt")
    (require "../subdirectory/aa.rkt")
    ```
    
    以当前文件的路径为当前路径,使用和shell类似的路径表达方式来确定需要导入的模 块的名字,记住要带扩展名

2.  当前文件的子模块: `(require 'name)`

3.  标准模块: `(require racket)`

4.  子模块: `(require (submod "aa.rkt" submod-name))`

### provide

  - 导出所有: `(provide (all-defined-out))`
  - 只导出通过require引入的绑定: `(provide (all-from-out))`
  - 导出除指定的外所有的绑定: `(provide (except-out name ...))`
  - 重命名: `(provide (rename-out [orgn-id export-id]))`

some example:

``` scheme
(provide run run-all)
(provide (all-defined-out))
```

# typed racket

## sample program

``` scheme
#lang typed/racket
(struct: pt ([x : Real] [y : Real]))

(: distance (pt pt -> Real))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
```

## Types

### Basic Types

1.  Number
2.  Char
3.  String
4.  Boolean

### Function Type

(Number -\> Number) (String String -\> Number)

### Union Type

当一个类型有几种变种时, 应该使用Union Type

``` scheme
#lang typed/racket
(define-type Tree (U leaf node))
(struct: leaf ([val : Number]))
(struct: node ([left : Tree] [right : Tree]))

(: tree-height (Tree -> Integer))
(define (tree-height t)
  (cond [(leaf? t) 1]
        [else (max (+ 1 (tree-height (node-left t)))
                   (+ 1 (tree-height (node-right t))))]))

```

上面的Tree就包含两种类型, Node与Leaf

### Recursive Type

``` scheme
(define-type BinaryTree (Rec BT (U Number (Pair BT BT))))
```

### Subtyping

任何类型都是Any的子类型.

### Polymorphism

``` scheme
(: list-length (All (A) ((Listof A) -> Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))
```

注意上面的 `All`.

# Footnotes
