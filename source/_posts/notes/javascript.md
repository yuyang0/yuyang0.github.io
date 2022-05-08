---
title: js笔记
date: 2014-02-18 11:54:17
tags:
- js
- lang
categories:
- notes
---
# javascript文档

几份不错的文档：[JavaScript
Garden](http://bonsaiden.github.io/JavaScript-Garden/) ,火狐开发者社区的js
[tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide?redirectlocale%3Den-US&redirectslug%3DJavaScript%252FGuide)

# 基本类型

## type的基本常识

1.  数字, 字符串, boolean, null, undefined是js的基本类型，它们不是object, 其
    它所有的值都是object, 数字, 字符串, bookean这三种类型有对应的包装对象 （wrapper
    object），当你访问字符串的方法时，实际上会自动的创建一个临时的 字符串包装对象，就像调用了new
    String(s),当调用完成后这个临时对象会被丢弃，
    对于数字（Number），bool（Boolean）都是同样的原理
2.  数字，字符串，bool都是不可变的，而object是可变的.
3.  类型转换： 2 + '2a' = '22a'(操作符是+，则自动将数字转换为字符串，如果是其
    它操作符则尝试将字符串转为整数，'22'转换为22,
    但'2a'无法转换，这一点和 parseInt不同)

## 数字

不区分整数与浮点数

``` javascript
var x = 2;
var x = 2.22;
```

有一些数学函数，用来处理数字：

``` javascript
Math.pow(2,53)         //=> 9007199254740992: 2 to the power 53
Math.round(.6)         //=> 1.0: round to the nearest integer
Math.ceil(.6)         //=> 1.0: round up to an integer
Math.floor(.6)         //=> 0.0: round down to an integer
Math.abs(-5)         //=> 5: absolute value
Math.max(x,y,z)         //Return the largest argument
Math.min(x,y,z)         //Return the smallest argument
Math.random()         //Pseudo-random number x where 0 <= x < 1.0
Math.PI         //π: circumference of a circle / diameter
Math.E         //e: The base of the natural logarithm
Math.sqrt(3)         //The square root of 3
Math.pow(3, 1/3)         //The cube root of 3
Math.sin(0)         //Trigonometry: also Math.cos, Math.atan, etc.
Math.log(10)         //Natural logarithm of 10
Math.log(100)/Math.LN10         //Base 10 logarithm of 100
Math.log(512)/Math.LN2         //Base 2 logarithm of 512
Math.exp(3)         //Math.E cubed
```

### 日期

``` javascript
var then = new Date(2010, 0, 1); // The 1st day of the 1st month of 2010
var later = new Date(2010, 0, 1, // Same day, at 5:10:30pm, local time
                     17, 10, 30);
var now = new Date();   //The current date and time
var elapsed = now - then;  //Date subtraction: interval in milliseconds

later.getFullYear() // => 2010
later.getMonth() // => 0: zero-based months
later.getDate() // => 1: one-based days
later.getDay() // => 5: day of week. 0 is Sunday 5 is Friday.
later.getHours() // => 17: 5pm, local time
later.getUTCHours() // => hours in UTC time; depends on timezone

later.toString()   // => "Fri Jan 01 2010 17:10:30 GMT-0800 (PST)"
later.toUTCString()   // => "Sat, 02 Jan 2010 01:10:30 GMT"
later.toLocaleDateString()   // => "01/01/2010"
later.toLocaleTimeString()   // => "05:10:30 PM"
later.toISOString()   // => "2010-01-02T01:10:30.000Z"; ES5 only
```

## 字符串

字符串使用单引号或者双引号,建议使用单引号,因为这样可以更好的和html配合

``` javascript
var x = 'hello world'; // 字符串单双引号等价
var x = "hello world";
```

js的字符串实际上就是一串16位(2个字节)的值，因为js使用UTF-16来encoding unicode字符，所以如果一个uncode
point使用utf-16编码后有3个字节，那么使用length时该字符就是2个长度,js的字符串操作基本都是以16位的值为基础，
而不是以逻辑上的字符为基础的

### 常见字符串处理函数

``` javascript
var s = "hello, world"     // => 3: position of first "l" at or after 3
s.length             // 12, the length of string(this is property, not method)

s.charAt(0)    // => "h": the first character.
s.charAt(s.length-1)    // => "d": the last character.
s.substring(1,4)    // => "ell": the 2nd, 3rd and 4th characters.
s.slice(1,4)    // => "ell": same thing
s.slice(-3)    // => "rld": last 3 characters
s.indexOf("l")    // => 2: position of first letter 1.
s.lastIndexOf("l")    // => 10: position of last letter 1.
s.indexOf("l", 3)    // => 3: position of first "l" at or after 3

s.split(", ")         //  ['hello', 'world']  an Array
s.replace("h", "H")   // Hello world, replace all instances
s.toUpperCase()       // HELLO WORLD
```

## bool

只能为true或者false,以下的值会自动转换为false：

1.  false
2.  null
3.  undefined
4.  "" (empty string)
5.  NAN (not a number)
6.  0, -0

除以上的值之外的所有的值，包括所有的对象都会自动转换为true, 可以通过 \!\! 来明 确的将一个值转换为bool

## undefined与null

前者表示没有定义，后者表示变量的值为空，eg：var x;(x为undefined)

# 正则表达式

## Create RegExp Object

正则表示式是对象(RegExp对象), 有两种定义方法:

1.  `var pattern = /../;`, **无引号,不是字符串**.
2.  `var pattern = new RegExp(...);`

**正则表达式要写成一行, 因为正则表达式中空格是非常重要的**.

## flags

正则表达式可以指定标志, 比如 `/../g`, `/../i`.

1.  g : 全局模式
2.  i : 忽略大小写模式

## group

正则表达式可以分组. 分组有以下几类:

1.  (..) : 这种分组是捕获型分组, 每一个捕获型分组都会有一个编号, 这种编号是从 1开始, 如果一个捕获型分组的编号是 `2`,
    且最后的结果是 `result`, 那么该分 组匹配的文本就是 `result[2]`.
2.  (?: ..): 以 `?:` 开头, 非捕获型分组, 这种分组不会干扰捕获型分组的编号, 它 匹配的文本也不会出现在最终的结果中.

## RegExp method

这是regexp的方法

1.  `.exec(str)`: 返回一个数组, 数组的第一个元素是匹配的完整字符串,接下来的元素 是所有的捕获型分组,
    注意全局模式对exec不起作用, 它只返回第一个匹配.
2.  `.test(str)`: 如果str中包含能被匹配的字符串, 那么返回true, 否则返回false.

## String method

这是字符串的方法

1.  `.search(regexp)`: 返回匹配字符串的第一个字符在原字符串中的index, 无匹配 那么返回 -1,
    全局模式对search无用.
    
    ``` javascript
    var s = "JavaScript is fun";
    s.search(/script/i) // Returns 4
    s.search(/a(.)a/)   // Returns 1
    ```
    
    如果 `search` 的第一个参数不是正则表达式及而是一个字符串, 那么先将它转换 为正则表达式(将字符串传递给 `new
    RegExp(..)`)

2.  `.replace(regexp, new_str)`: 返回替换后的新字符串, 如果regexp是全局模式,
    那么会替换所有的匹配的字符串, 否则只替换第一处. 如果regexp中使用了分组,那
    么可以在new\_str中通过 $1, $2 … $n 来引用第一个, 第二个一直到第n个分组匹 配的字符串.
    
    ``` javascript
    text.replace(/javascript/i, "JavaScript");
    "Doe, John".replace(/(\w+)\s*,\s*(\w+)/, "$2 $1"); // => "John Doe"
    ```
    
    如果 `replace` 的第一个参数不是正则表达式及而是一个字符串, 那么直接使用字 符串字面值匹配

3.  `.match(regexp)`: 返回一个数组, 如果regexp是全局模式, 那么数组元素就是所 有匹配的字符串,
    如果没有使用全局模式, 那么数组的第一个元素是匹配的字符串, 接下来的元素是所有的捕获型分组,
    具体可以看下面的例子.
    
    ``` javascript
    // regexp has 'g' attribute
    "1 plus 2 equals 3".match(/\d+/g)  // => ["1", "2", "3"]
    
    // regexp doesn't have 'g' attribute
    var url = /(\w+):\/\/([\w.]+)\/(\S*)/;
    var text = "Visit my home page at http://www.isp.com/~david";
    var result = text.match(url);
    if (result != null) {
      var fullurl = result[0];   // Contains "http://www.isp.com/~david"
      var protocol = result[1];   // Contains "http"
      var host = result[2];   // Contains "www.isp.com"
      var path = result[3];   // Contains "~david"
    }
    ```
    
    如果 `match` 的参数不是正则表达式, 那么先转换为正则表达式.

4.  `.split(regexp)`:

<!-- end list -->

``` javascript
"123,456,789".split(",");          // => ['123', '456', '789']
"1, 2, 3, 4, 5".split(/\s*,\s*/); // => ["1","2","3","4","5"]
```

不会将字符串字面值转换为正则表达式, 和 `replace` 类似.

# Array

## 定义

定义数组可以使用两种方法: Array与\[\],但是推荐\[\],因为像 new Array(3)这样的代
码，它会返回一个空数组，可是却将这个数组的length设置为3,这是一个令人困惑的
特性

``` javascript
[1, 2, 3]; // 结果: [1, 2, 3]
new Array(1, 2, 3); // 结果: [1, 2, 3]

[3]; // 结果: [3]
new Array(3); // 结果: []
new Array('3') // 结果: ['3']

// 译者注：因此下面的代码将会使人很迷惑
new Array(3, 4, 5); // 结果: [3, 4, 5]
new Array(3) // 结果: []，此数组长度为 3

```

## 遍历数组

不要使用for…in, 而要使用如下代码:

``` javascript
var list = [1, 2, 3, 4, 5, ...... 100000000];
for(var i = 0, l = list.length; i < l; i++) {
    console.log(list[i]);
}
```

原因是for … in会遍历整个原型链，效率不高

## 数组方法

| 方法名     | 作用                          | 返回值 | 例子                                              |
| ------- | --------------------------- | --- | ----------------------------------------------- |
| concat  | 将数组连接起来                     | 新数组 | \[1,2\].concat(\['a','b'\]); =\>\[1,2,'a','b'\] |
| join    | 构造一个字符串                     | 字符串 | \['a','b','c'\].join(','); =\>'a,b,c'           |
| pop     | 移除最后一个元素(原地改变)              | 数组  | \['a','b','c'\].pop(); =\> 'c'                  |
| push    | 将元素插入数组尾部(原地改变)             | 数组  |                                                 |
| reverse | 反转元素顺序(原地改变)                | 数组  | \[1,2,3\].reverse() =\>\[3,2,1\]                |
| sort    | 排序数组,默认比较字符串,可以传递比较函数(原地改变) | 数组  | \[4,15,28\].sort() =\>\[15,28,4\]               |
| slice   | 选取数组的一段                     | 新数组 | \['a','b','c'\].slice(0,1) =\>\['a'\]           |

# objects(对象)

## 基本解释

object有点类似于关联数组, js中除了数字，字符串，bool，null，undefined之外都
是对象，数组，函数等等都是对象，对象有属性名，与属性值，数组的属性名是一些小
的连续的整数，这也是适合用数组的场景，其它的地方都应该用对象. object的定义如 下:

``` javascript
var foo = {
  name: 'Kitten',
  age : 12
};

foo.name; // kitten
foo['name']; // kitten

var get = 'name';
foo[get]; // kitten

foo.1234; // SyntaxError
foo['1234']; // works
```

两种访问方法foo.name与foo\['name'\]， 推荐前者(前提是name必须是合法的js标识
符)，和python的dict不同，{}中的name不要加引号，因为这里严格的说是object的 属性，而不是key

## 原型

对象对属性名的搜索有一定的规则, 一般情况下通过字面值构建的对象都会与
Object.prototype链接，也就是以Object.prototype为原型,所以如果一个属性在对象
中没有找到，那么她会自动到 Object.prototype中找，这样我们就可以给对象进行扩
充，比如给object.prototype中添加一个方法，那么每一个对象都可以调用, 同时也可
以给指定一个对象的原型,比如下面这个函数:

``` javascript
if (typeof Object.beget !== 'function') {
  Object.beget = function(o){
    var F = function(){};
    F.prototype = o;
    return new F();
  };
}
```

Object.beget 会返回一个新对象, 这个对象会以调用者指定的 o 为原型.

## json

1.  JSON.stringify: 将一个对象转换为字符串(serialize)
2.  JSON.parse: 将一个字符串转换为对象.

# control flow

基本和C语言类似，while，for，do…while，if…else if…else,switch…case, break,
continue都和C语言差不多，
break,continue和C语言有个区别就是后面可以跟一个label，break后面跟label那么它就不是终止最内层循环，而是终止
label指定的循环，eg：

``` javascript
var x = 0;
var z = 0
labelCancelLoops: while (true) {
    console.log("Outer loops: " + x);
    x += 1;
    z = 1;
    while (true) {
        console.log("Inner loops: " + z);
        z += 1;
        if (z === 10 && x === 10) {
            break labelCancelLoops;
        } else if (z === 10) {
            break;
        }
    }
}
```

break
labelCancelLoops会终止最外层那个循环，continue后面跟label也和这类似，continue如果有label，那么它会终
止当前循环，而开始新一轮的label指定的循环

  - for (key in obj) : 对于C语言类似的数组，它会获得index，而对于关联数组则会获得key,
    所以取值需要obj\[key\]，但是这种循环不建议使用，因为它实际是遍历原型链，所以你无法
    保证顺序，也会做很多无用功
  - for each (var item in obj): item会赋值为值，而不是key

# function

函数也是对象, 每一个函数对象都会链接到 Function.prototype对象上, 也就是以
Function.prototype对象为原型,而Function.prototype又会链接到
Object.prototype 上. 同时每一个函数对象有一个prototype属性, 每一个通过new创建的对象都会链接到 这个属性
但是这个属性和函数对象本身没关系.

## definition

``` javascript
function square(number) {
  return number * number;
}

var square = function(number){  // recommend
  return number * number;
}
```

## 四种调用方式

### method调用模式

``` javascript
var myObject = {
  value: 0,
  increment: function(inc){
    this.value += typeof inc === 'number' ? inc : 1;
  }
};
myObject.increment();           // myObject.value is 1
myObject.increment(2);           // myObject.value is 2
```

这样调用时this会自动绑定到该对象, 在上例中就是 myObject.

### 函数调用模式

普通的函数调用: 比如 `var sum = add(3, 4);`, 在这种调用方式中, 在函数内部 (add)
this会被绑定到全局对象也就是 window对象

### 构造器调用模式(不推荐)

实际是对OOP的一种不必要且蹩脚的模拟, 很晦涩难懂, 这种调用方式是使用 new 操 作符来调用. 这样调用时函数会有截然不同的行为.

``` javascript
var Quo = function(string){
  this.status = string;
};

Quo.prototype.get_status = function(){
  return this.status;
}
var myQuo = new Quo("confused");
myQuo.get_status()              // the result is "confused"
```

我解释下上面的代码: 首先 Quo 是一个函数对象(构造器), 当对这个对象使用 new 操作符时会创建一个新的对象也就是myQuo,
myQuo会以 Quo.prototype 为原型, 同时 在 Quo运行时, this会绑定到正在创建的那个对象也就是 myQuo.

一般来说构造器函数要以大写开头的名字命名, 同时在构造器内部不会明确的使用 return语句, 但是一旦明确的使用了return,
那么最后的结果就是return后的那个对 象了. 内置的Number, String, Regex,
Date等等实际都是构造器函数.

在一般的 OOP 语言中是区分类与对象, 所以上述代码实际就是在模拟类, 可是js完全 不需要类, 所有这种模拟是不必要的

### apply调用模式

每一个函数都有一个apply方法, 该方法接受2个参数, 第一个参数是传递给 this的, 第二个参数是一个参数数组,
和lisp的apply有点类似

``` javascript
var arr = [1, 2];
var sum = add.apply(null, arr); // sum is 3
```

在上例的这次add函数的apply调用中, 在add函数的内部 this 被绑定到 null.

## 基本特性

js的函数是first class object，所以它可以作为参数传递，也可以作为返回值返回， 支持闭包与匿名函数，使用词法作用域,
它的很多地方借鉴了lisp的特性，下面是一些 示例代码：

``` javascript
 // define a function if num==0
 var myFunc;
 if (num == 0){
   myFunc = function(theObject) {
     theObject.make = "Toyota"
   }
 }
// closure
// The outer function defines a variable called "name"
 var pet = function(name) {
   // The inner function has access to the "name" variable of the outer function
   var getName = function() {
     return name;
   }
   // Return the inner function, thereby exposing it to outer scopes
   return getName;
 },
     myPet = pet("Vivie");
myPet();                            // Returns "Vivie"
```

## 作用域

1.  `var` 用来声明变量, 每一个变量都应该先声明, 后使用.
2.  如果省略了 `var` 那么会创建一个全局变量, 不要省略var是一个好的做法, 而且 特别要注意打字错误,
    jslint会提示没有用var声明的变量 **特别注意**.
3.  js 没有块作用域, 也就是说不是每一对{} 都会创建一个新的作用域,这和C,java不 同.
4.  其它规则和lisp类似

<!-- end list -->

``` javascript
scope = "global";         // Declare a global variable, even without var.
var scope2 = "global";    // Declare another global variable.

function checkscope2() {
  scope = "local";          // Oops! We just changed the global variable.
  var scope2 = "local";     // Declare a local varible with the same name.

  myscope = "local";        // This implicitly declares a new global variable.
  return [scope, myscope];  // Return two values.
}
checkscope2()             // => ["local", "local"]: has side effects!
scope                     // => "local": global variable has changed.
myscope                   // => "local": global namespace cluttered up.
```

js的作用域设计的比 python好, 因为变量需要声明, 所以就可以将创建绑定与修改变 量的值区分开, 也就不需要引入 `global`
这样的关键字, 也就不需要对全局作用域特 殊对待.

## 给类型增加方法

``` javascript
Function.prototype.method = function (name, func){
  if (!this.prototype[name]) {
    this.prototype[name] = func;
  }
};
```

给 String 增加 trim方法:

``` javascript
String.method('trim', function(){
  this.replace(/^\s+|\s+$/g, '');
});
```

## 内置的函数

1.  eval:运行js代码, 不推荐使用.
2.  isFinite: test a number if it is a finite number
3.  isNaN: is not a number
4.  parseInt,parseFloat: 将字符串转换为整数或者浮点数

# 一些js技巧

1.  在每个应用中仅创建一个全局的对象,然后将所有的函数都变成这个全局对象的属性, 减少与其它应用程序或者类库产生相互影响的可能性.

2.  每个语句结束要插入分号,不要依赖js解释器的自动插入分号功能.

3.  不要使用=`, !` 而应该一直使用===与\!== .

4.  if, while这样的语句一定要使用 {}, 即便只有一条语句.

5.  不要使用 `++` 与 `--` , 因为这让程序不易理解.

6.  不要依赖 switch 中 case 的贯穿, 也就是说, 每一个case都要一个break.

7.  尽量不要使用位操作(&, \!, ^),因为js没有整数类型, 它只有双精度浮点数

8.  函数的声明应该用:
    
    ``` javascript
    var foo = function(){...};
    ```
    
    而不是
    
    ``` javascript
    function foo(){...}
    ```
    
    因为第二种方式会使得不管函数定义在什么地方,它都会被移到被定义时的作用域的 最开头, 这违背了函数先定义后使用规则(scheme,
    python都遵循该规则).

9.  尽量不要使用 new Object, new Array, 应该使用 {} \[\] 来代替

10. 不要使用new运算符.
