---
title: Clojure笔记
date: 2014-02-18 11:14:17
tags:
- clojure
- lisp
- lang
categories:
- notes
---
# Clojure Forms

## Boolean

  - typical value: true, false
  - useful function:
    1.  not
    2.  and
    3.  or

## Nil

  - typical value: nil 只有false与nil会被计算为false,其它的都为true

## Character

  - typical value:  .̧̱..

## Number

  - typical value: 1 2
  - useful function:
    1.  \+, -, \*
    2.  /(分数形式),quot(商),rem(余数)
    3.  inc,dec
    4.  min, max
    5.  \=`,<,<`,\>,\>=
    6.  zero?,pos?,neg?,number?

## Symbol

  - typical value: user/foo

## String

  - typical value: "hello"
  - useful function:
    1.  str: 拼接多个字符串
    2.  subs: 子字符串(0为开始下标)
    3.  string?
    4.  print & println: 后者自动添加一个新行

### regex <span class="tag" data-tag-name="regex"><span class="smallcaps">regex</span></span>

1.  re-pattern:创建一个正则表达式, `(re-pattern "[abcd]")` 等价于 \#"\[abcd\]",

## KeyWord

  - typical value: :tag :doc

## List

  - typical value: (+ 1 3) (println "foo")

## Vector

  - typical value: \[1 2 3\]

## Map

  - typical value: {:key1 val1, :key2 val2 …} {"Lisp" "McCarthy"
    "Clojure" "Hickey"}

## Set

  - typical value: \#{:val1 :val2 …}

# seq operation

clojure的sequence都是不可变的.

  - first: like car in scheme
  - rest: like cdr in scheme
  - cons: like cons in scheme (Construct)
  - conj: 将单个元素插入seq,对于list是插入到最前面,对于vector是插入最后面
  - into: 将后一个seq插入到前一个seq,对list是插入最前面,对vector是插入最后面

## seq library

下面的函数对任何类型的sequence都有效, 而且这些操作除极少数外基本都返回 lazy sequence.

  - empty? : same as null? in scheme.

  - seq : 如果coll为空则返回 nil, 否则返回一个seq.

  - range: (range start? end step?)
    
    ``` clojure
    (range 5)
    ;;=> (0 1 2 3 4)
    ```

  - repeat:
    
    ``` example
    (repeat 5 "a")          => ("a" "a" "a" "a" "a" )
    (repeat "a")            => return an infinite seq
    (take 5 (repeat "a"))   => ("a" "a" "a" "a" "a" )
    ```

  - (iterate f x): f可以看做是一个后继函数,返回一个无限序列
    
    ``` example
    (take 5 (iterate inc 1))         => (1 2 3 4 5)
    ```

  - take: 从一个无限序列中抽取指定个数的元素组成序列

  - cycle:返回一个无限的序列
    
    ``` example
    (take 10 (cycle (range 3)))    => (0 1 2 0 1 2 0 1 2 0)
    ```

  - interleave:
    
    ``` example
    (interleave '(1 2 3) '(a b c) '(x y z))     => (1 a x 2 b y 3 c z)
    ```

  - interpose:
    
    ``` example
    (interpose \, ["aa" "bb" "cc"])     => ["aa" \, "bb" \, "cc" \,]
    ```

  - (join sep seq) : 返回将seq中的元素用sep分割后的字符串

  - list:生成一个list

  - vector: 生成一个vector

  - hash-set:

  - hash-map:

  - filter:

  - map:

  - reduce:

  - split-at:将一个seq根据index分割为2个seq

  - split-with: 将一个seq根据一个predictive function的返回值分割为2个seq,true
    为一个seq,false为一个seq

  - every?

  - some

  - not-every?

  - not-any?

# 基本语法

## function definition

1.  匿名函数(fn) `(fn [x y] (+ x y))` 创建一个匿名函数, `fn` 和 `lambda` 类似,fn还有一个简
    写形式 `#(+ %1 %2)`.如果只有一个参数,那么可以用 % 代替 %1

2.  def: 可以将一个匿名函数与一个name关联起来,和 `scheme` 中的 `define` 类似
    
    ``` clojure
    (def my-add (fn [x y] (+ x y)))
    ```

3.  defn: 是def 与 fn 的简写
    
    ``` clojure
    (defn my-add [x y] (+ x y))
    ```

4.  参数的解构
    
    ``` clojure
    (defn my-add [x y [a b]]
      (+ x y a b))
    (my-add 1 2 [3 4])
    ; => 10
    ```

5.  Arities(可以看做是函数多态) 根据参数的不同而执行不同的动作
    
    ``` clojure
    (defn square-or-multiply
      "squares a single argument, multiplies two arguments"
      ([] 0)
      ([x] (* x x))
      ([x y] (* x y)))
    ```
    
    这应该也是为什么clojure中doc string是放在函数名之后而不是参数名之后的原因

6.  递归: 由于jvm的关系,clojure不会自动进行尾递归优化,在尾递归的地方,你应该明 确的使用 `recur`
    这个关键词,而不是函数名
    
    ``` clojure
    (defn my-add [x y]
      (if (zero? x)
        y
        (my-add (dec x) (inc y))))
    
    (defn my-add [x y]
      (if (zero? x)
        y
        (recur (dec x) (inc y))))
    ```
    
    第一个不会进行尾递归优化,第二个会进行尾递归优化

7.  loop:和 `recur` 配合可以实现和循环类似的效果
    
    ``` clojure
    (loop [i 10 j 10]
      (if (zero? i)
        j
        (recur (dec i) (inc j))))
    ```
    
    和scheme中使用 let 创建一个函数很相似
    
    ``` scheme
    (let loop ([i 10] [j 10])
      (if (zero? i)
          j
          (loop (- i 1) (+ j 1))))
    ```

8.  curry 只提供部分参数给函数,比如我们可以这样定义 `add3`
    
    ``` clojure
    (def add3 (partial + 3))
    (add3 4)
    ;=> 7
    ```
    
    partial 接受有一个函数以及部分参数,返回一个函数

9.  comp: 可以生成一个函数,f(g(x))等价于 `((comp f g) x)`

## 基本控制结构

### if

### cond

### let

### letfn

## 模块与namespace

1.  require: 导入clojure模块

2.  use: 导入clojure模块,与require的区别是,use会将指定模块的名字导入当前的
    namespace,所以在引用时就不需要添加模块名作为前缀,而require则需要,一般情况
    下推荐use

3.  import: 导入java的类
    
    ``` clojure
    (import 'java.util.Date)
    ; (new Date)
    (import '(java.util.regex Pattern Matcher))
    ; only import Pattern and Matcher in java.util.regex
    ```

4.  ns:创建一个命名空间,ns是一个宏,所以后面的参数不需要quote,而且注意后面的
    require,use,import是以keyword的形式给出
    
    ``` clojure
    (ns com.example.library
      (:require [clojure.contrib.sql :as sql])
      (:use (com.example one two))
      (:import (java.util Date Calendar)
               (java.io File FileInputStream)))
    ```

# State

## ref

1.  ref

2.  deref(@)

3.  ref-set: 需要使用 dosync来避免竞争条件

4.  dosync: 被dosync包裹的表达式要么全部执行成功,要么都不执行,并且保证每一步
    都不出现竞争条件,和数据库的存储过程很类似,实现了ACI,数据库的存储过程一般
    实现了ACID.
    
    ``` clojure
    (def current-track (ref "Venus, the Bringer of Peace"))
    ;; -> #'user/current-track
    (def current-composer (ref "Holst"))
    ;; -> #'user/current-composer
    (dosync
     (ref-set current-track "Credo")
     (ref-set current-composer "Byrd"))
    ```

5.  alter : `(alter ref update-fn & args...)`, `@ref` 作为update-fn的第一个参
    数(这也是为什么下面的代码要用conj代替cons的原因),args作为剩余的参数,alter
    可以看作是dosync,ref-set,deref的简写
    
    ``` clojure
    (def message (ref ()))
    (defn navie-add-message [msg]
      (dosync (ref-set message (cons msg (deref message)))))
    ;;; identical
    (defn add-message [msg]
      (alter message conj msg))
    ```

## atom

atom和ref很类似,但是atom当你仅仅只是需要原子的更新单个值时,使用更简便.

1.  atom: `(def (atom ()))`
2.  deref(@)
3.  reset\!: 更新原子的值
4.  swap\! : `(swap! r update-fn & args)` 用一个函数调用来生成新的值

# Macro

## 几个简写

1.  反引号(\`) : 和scheme相同

2.  \~, \~@ 和scheme的 , ,@ 的含义相同,之所以用 \~ 是因为clojure中 , 与空格等价

3.  id\# :在一个标识符背后加上 \# 意味着生成一个唯一的symbol, 比如 foo\# 实际可 能就是 foo\_004
    可以看作是let与gensym的等价物,这在避免符号捕捉时很有用.
    
    ``` clojure
    (defmacro and
      ([] true)
      ([x] x)
      ([x & rest]
         `(let [and# ~x]
            (if and#
              (and ~@rest)
              and#))))
    ```
    
    注意上面的 and\# 不需要使用 \~ 来求值,因为 and\# 本身就是一个独一无二的符号

# 调用java

1.  new : 创建一个对象. `(new String)`
2.  . : `(. target name & args)`
3.  set\! : `(set! (. target name) value)`

上面的3个操作符已经足够,但是为了方便,还有以下几个操作符

1.  ClassName/field: 只对静态变量或者方法有效
    
    ``` clojure
    Integer/MIN_VALUE
    ; => -2147483648
    (Integer/parseInt "101")
    ; => 101
    ```

2.  .method : `(.method object args)` 可以看做等价于 `(. object method args)`,
    但是要记住 .method并不是真正的第一类函数对象,所以你不能直接将它传递给 map,filter这类函数
    
    ``` clojure
    (map #(.toUpperCase %) ["one" "two" "three"])
    ; => ("ONE" "TWO" "THREE")
    ```
