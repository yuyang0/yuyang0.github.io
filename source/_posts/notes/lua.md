---
title: lua笔记
date: 2014-02-17 11:54:17
tags:
- lua
- lang
categories:
- notes
---
# table

实际上是一个hash表, 把python中dict与list杂揉到了一起,设计的并不好.

## 初始化

``` lua
local tbl = {aa=1, bb=2, "l1", "l2", "l3"}
print(#tbl)    -- return 3
local tbl = {aa=1, bb=2}
print(#tbl)    -- return 0
```

表初始化时如果你没有指定key, 那么默认使用数字为key, 所以"l1", "l2","l3"的key 分别是1,2,3.
也就是说被当成了list, 我建议将dict, list明确的区分开来, 有一个长 度操作符\#
来获得list的长度,它实际上是获得最大的数字key. 举个例子，如果一个
table的数字key有这些：1,2,3,4,6,7, 那么\#操作符会返回4, 因为中间有一个空洞的关
系，\#检查到5的时候发现为nil就停止了 \#u操作符大致等价于下面的函数.

``` lua
function get_len(tbl)
   local i = 1
   while true do
      if not tbl[i] then
         return i-1
      end
      i = i+1
   end
end
```

## 遍历table

pairs，ipairs都会返回一个迭代器，只是pairs会迭代所有的key，ipairs只会迭代数字key。
所以为了方便起见，最好使用pairs

``` lua
for k, v in pairs(tbl) do
  dosomething()
end
```

# metatable

lua的每一个表或者userdata都可以有单独的元表, 其他类型的值则共享该类型的元表, 比如说所有的整数都拥有一个相同的整数类型的元表,
我们一般对表的元表比较感兴趣.

## 特殊方法

1.  \_\_index: **元表的\_\_index属性可以为函数或者表**, 给定表tbl以及键key,获得 value的流程如下:
      - tbl\[key\]: 如果为nil, 也就是说key不在tbl中, 那么看下面
      - 检查tbl的元表, 如果有元表且元表有\_\_index属性, 并且\_\_index属性的值是函数 或者表,那么看下面,
        否则返回nil
      - 如果\_\_index属性是一个函数, 那么这样调用 \_\_index(tbl, key)
      - 如果\_\_index属性是一个表, 那么返回\_\_index\[key\]
2.  \_\_newindex: **元表的\_\_newindex属性可以为函数或者表**,只有对表中不存在的
    index赋值时才会调用元表的\_\_newindex方法.

# 环境

## \_ENV(lua5.2)

lua将每一个chunk转换为一个匿名函数, 匿名函数中的自由变量(也就是全局变量)会转 变为对表\_ENV的引用,比如:

``` lua
var1 = var2 + 1
```

很显然var1, var2是全局变量, 那么这个chunk实际会转换为这个样子

``` lua
_ENV = _G
return function ()
   _ENV.var1 = _ENV.var2 + 1
end
```

从上面的代码你可以看到, 全局变量的访问与赋值实际就是对\_ENV表的相应key的访问与 修改,
这个\_ENV实际就是为了实现静态作用域而必须和函数绑定到一起的环境,
如果没有 这个环境那么自由变量就只能在动态执行过程中决定,这就成了动态作用域, 默认情 况\_ENV是被赋值为\_G,
但是你可以修改\_ENV, 同时匿名函数中的自由变量还有一个名字 叫 **上值(upvalues)**,
很显然var1, var2就是上值

## setfenv/getfenv

\_\_ENV机制是lua5.2引入的, 在luajit中也没有实现, setfenv实际上也可以设置函数的 环境,
第一个参数是一个函数,也可以是数字,它代表当前栈上的函数,比如说1就代表目
前正在执行的函数.

# OOP

可以使用表来模拟OOP的行为.

``` lua
Account = {
   balance=0,
   withdraw = function (self, v)
      self.balance = self.balance - v
   end
}

function Account:new (o)
   o = o or {}
   -- create object if user does not provide one
   setmetatable(o, self)
   self.__index = self
   return o
end

function Account:deposit (v)
   self.balance = self.balance + v
end

-- 等价,冒号是语法糖，可以看做是自动添加self
function Account.deposit (self, v)
   self.balance = self.balance + v
end
```

在new中创建一个表o, 同时将o的metatable设为self(也就是Account), 同时设置 self的\_\_index属性,
这样o中不存在的属性都会去Account表中找. 若是新对象没有指定
balance值,那么Account中的balance会新对象的默认值.
同时注意冒号是一个语法糖, 也 就是说 `o:deposite(100)=等价于
=o.deposite(o, 100)`,

# package

lua中比较好的package设施应该满足以下几点:

1.  不要因为漏写local而污染全局环境
2.  不要在每一个api前添加包名前缀

推荐的做法如下:

``` lua
-- 将模块用到的全局变量赋值给local变量
local print = print
local error = error
local ngx = ngx
local setmetatable = setmetatable

local _M = {}
-- 修改当前chunk的匿名函数的环境为_M, 这样该chunk的所有全局变量都会变成_M的元素
-- 这样可以避免漏掉local导致的函数变成全局函数的问题.
if setfenv then                 -- for lua5.1 and luajit
   setfenv(1, _M)
elseif _ENV then                -- for lua5.2 or newer
   _ENV = _M
else
   error("both setfenv and _ENV are nil...")
end

-- private function
local function afunc()
   dosomething
end

-- public method
function new()
   do_something
end

-- safety set, 禁止添加新属性
local module_mt = {
   __newindex = (
      function (table, key, val)
         error('Attempt to write to undeclared variable "' .. key .. '"')
                end),
}

setmetatable(_M, module_mt)

return _M
```

# 一些注意事项

1.  chunk: chunk是lua执行代码的最小单位,一个chunk必定是一个block, 你可以认为
    chunk是最上层的block,一般来说一个文件中的所有lua代码是一个chunk, 在交互环
    境下每一行(前提是这一行是一个完整的语句)都是一个chunk
2.  list的下标从1开始
3.  lua的不等于是 \~=, 不是 \!=
4.  lua没有++,–以及+=, -= 这样的操作符,所以老老实实的写i=i+1
5.  lua的字符串拼接是 .. ,不是+, 它会自动将数字转换为字符串
