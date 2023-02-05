---
title: golang笔记
date: 2014-02-11 11:54:17
tags:
- golang
- lang
categories:
- notes
---

# go代码规范

1.  使用驼峰命名法, 私有名字以小写字母开头，公有名字以大写字母开头, 也就是说 **小写
    字母开头，那么只在本包内可见，大写字母开头才能在包外可见**
2.  大括号不要换行
3.  包名最好文件夹名相同, 并且为小写

# 变量

变量的声明主要有两种方法

1.  var x type
2.  x := val

使用var声明引用变量是不会分配内存的，因此到底需不需要调用make来初始化则视情况而 定,
**map需要调用make来初始化，slice不需要**.
这是因为即使是一个空的map，也需要一 些额外的内存来存储信息,
slice则不需要。

``` go
var s []int
s = append(s, 1)      // ok

var m map[string] int
m["aa"] = 1           // error

var m map[string] int
m = make(map[string]int)
m["aa"] = 1           // ok
```

# 基本类型

go引用提供了很多基本类型，这些基本类型分为两类：值类型和引用类型。 **map，channel， slice,interface,
function，method是引用类型，其它的包括array，struct都是值类型**. 这两种类型的不同在函数调用时会表现出不同。

## nil

只能赋值给 **指针** 和 **引用** 类型的变量. 实践中主要用来指示slice，map，interface
是未初始化的值。虽然nil和空的slice和map有很多相似之处，比如调用len会返回0，但是
他们并不是一回事。看下面的例子。

``` go
var slice []int            // slice == nil 返回true
slice = make([]int, 0)     // slice == nil 返回false

var mm map[string]int      // mm == nil 返回true
mm = make(map[string]int)  // mm == nil返回false
```

**slice, map, interface只能与nil进行比较**

``` go
var ss []int
ss == nil          // ok
ss == []int{1, 2}  // error
```

## bool

只能取值true，false。不能通过其它类型转换为bool，所以 `aa:=bool(1)` 这样的语句是 错误的。

## 整数

可以取值int, uint, int8,int16,int32, int64, uint8(byte), uint16, uint32,
uint64. int, uint是平台相关的，可能为32或者64位。

## 浮点

可以为float32或者float64，浮点不能使用==来直接比较相等，可以使用math包的 `Fdim` 来处理

## 复数

## 字符

可以为byte或者rune(rune和int32是等价的)，前者是字节或者是unicode

## 字符串

**go中字符串不可变**, go中的字符串实际就是utf8编码的字节流，所以如果存储的全部是英
文字符那么他和字节流(`[]byte`)没什么两样，但是如果有汉字，那么需要注意，因为一个
汉字在utf8中需要3到4个字节，所以你可能需要使用 `[]rune(s)` 转换成unicode字符串

``` go
var str string // 声明一个字符串变量
str = "Hello world" // 字符串赋值
ch := str[0] // 取字符串的第一个字符
fmt.Printf("The length of \"%s\" is %d \n", str, len(str))
fmt.Printf("The first character of \"%s\" is %c.\n", str, ch)

for i := 0; i < len(str); i++ {    //遍历字节
  fmt.Println(i, str[i])
}
for i, ch := range str {           //遍历unicode
  fmt.Println(i, ch)
}
```

### fmt

这个包包括了很多字符串格式化的命令。这个包的函数命名有一些规律

1.  开头
      - E： 会返回一个error值
      - F: 会将得到的字符串写入一个io.Writer中
      - S：会将得到的字符串返回
      - 无：将得到的字符写入标准输出
2.  结尾
      - f: 意味着需要一个格式字符串
      - ln: 会在末尾追加一个换行符

如果没有格式字符串那么就以默认的形式显示

``` example
fmt.Errorf(format, args...)           // 返回一个错误值
fmt,Fprintf(writer, format, args...)  //向writer写入字符串
fmt.Printf(format, args...)           //向os.Stdout写入字符串
fmt.Sprintf(format, args...)       //返回一个字符串
```

### strings

标准库中的strings包含了许多常见的字符串处理函数。

``` go
strings.Split(ss, sep)
strings.SplitAfter(ss, sep)    //保留分隔符
strings.Join(xs, sep)          //使用sep将xs的元素连接成字符串
strings.ToLower(s)
strings.ToUpper(s)
strings.ToTitle(s)

strings.Trim(s, t)
strings.TrimLeft(s, t)
strings.TrimRight(s, t)

strings.HasPrefix(s, p)
strings.HasSuffix(s, t)

strings.Index(s, t)
```

### strconv

这个包可以将字符串和其它类型进行转换

## 数组

数组是一个定长的序列，创建了长度就不能更改了。在go中切片远比数组通用

``` go
[length]Type
[length]Type{val1, val2, ..., valN}
[...]Type{val1, val2, ... , valN}

var [32]byte // 长度为32的数组，每个元素为一个字节
var [2*N] struct { x, y int32 } // 复杂类型数组
var [1000]*float64 // 指针数组
var [3][5]int // 二维数组(3行5列)
var [2][2][2]float64 // 等同于[2]([2]([2]float64))

aa := [5]int {1, 2, 3, 4, 5}  // 1, 2, 3, 4, 5
aa := [5]int {1, 2}           // 1, 2, 0, 0, 0
aa := [...]int {1, 2, 3, 4, 5} //1, 2, 3, 4, 5
aa := [5]int{2:1, 3:2, 4:3}    //0, 0, 1, 2, 3
aa := [...]int{2:1, 3:2, 4:3}    //0, 0, 1, 2, 3
```

## 切片(slice)

切片是引用类型，在标准库中，所有的api都使用切片，创建切片和创建数组最大的区别是， 数组必须在\[\]中指定长度，切片则一直是空的

``` go
make([]Type, length, capacity)
make([]Type, length)
[]Type{}
[]Type{val1, val2, ..., valN}   // 四种创建切片的方法，都没有在[]中指定大小

var slice1 []int       // 若你不知道切片的长度，这样声明最合适，不要用make
slice1 := []int {1, 2, 3, 4, 5}
slice2 := arr[:]                //从数组创建，语法和python类似
slice3 := make([]int, 5)        // 5个元素的切片，都初始化为0
slice3 := make([]int, 5, 10)    // 5个元素的切片，都初始化为0,同时预留10个元素的空间
newSlice := append(slice1, 8, 9, 10)
newSlice := append(slice1, slice2...)

for idx, val := range slice {     // 遍历
  // do something
}
```

可以通过切片语法从字符串，数组，切片来创建切片，语法是python切片语法的子集，记住 是左闭右开就是了。
**特别要注意make函数的第二个参数，该参数指定了slice的初始长度，
也就是调用len返回的值,所以你创建新的空slice时，这个值应为0**.

## 字典(map)

``` go
var aa map[string] int           //声明
bb := make(map[string] int)       //声明并赋值
cc := make(map[string] int, 100)  //指定初始容量
cc["key1"] = 10                   //添加key1
delete(cc, "key1")                 //删除key1
value, ok := myMap["1234"]        //查找key1
if ok { // 找到了
  // 处理找到的value
}
len(cc)                           // cc包含的键值对的个数
for k, v := range cc {            // 遍历map

}
```

## channel

channel是goroutine之间的通信方式, 它是线程安全的,但是要注意,如果你传递的指针,那 么它仍然有可能出现竞争条件

1.  channel应该由发送端来关闭

2.  使用如下的代码来测试channel是不是已经关闭
    
    ``` go
    e, ok := <- channel
    if ok {
      // get a item
    } else {
      // channel is closed.
    }
    ```
    
    如果channel没有关闭，而且也没有元素可读，那么上面的代码会 **阻塞**.要不阻塞的测
    试，只能使用带default的select。

3.  使用range遍历一个channel的时候, 如果channel已经关闭,那么range会终止.

4.  channel的超时
    
    ``` go
    select {
    case err := <-c:
      // use err and reply
    case <-time.After(timeoutNanoseconds):
      // call timed out
    }
    ```

5.  quit channel是通知goroutine退出的最佳方式，在需要退出的时候close掉channel，这
    样任何goroutine只要使用这样的代码测试就可以了
    
    ``` go
    select {
    case <- quitChan:
      // quit
    default:
    }
    ```
    
    关闭的channel上调用任意多次 `<-` 都不会阻塞，所以不要使用带buffer的channel。

## struct

```go
type Person struct {
    name string
    age  int
}
p1 := Person{
  name: "Steve",
  age: 22
}
p2 := Person{"Steve", 22}
```

## interface

```go
type Abser interface {
	Abs() float64
}
```

### interface{}

type assertion(类型断言)

``` go
var anything interface{} = "hello"
aString := anything.(string)              // unsafe, may panic
if aString, ok := anything.(string); ok { // safe, check ok
  doSomething
}
// if you are not sure, use type switch
switch v := anything.(type) {
case string:
  fmt.Println(v)
case int32, int64:
  fmt.Println(v)
case SomeCustomType:
  fmt.Println(v)
default:
  fmt.Println("unknown")
}
```

# 内置函数

1.  append： 添加元素到切片, 使用 `aa=append(aa, ele)` 的语法。将一个切片追加到另 一个切片 `s1 =
    append(s1, s2...)` 也就是要放省略号。
2.  close： 关闭通道
3.  make：用来创建slice，map，channel。可以指定长度和容量
4.  delete：从map中删除项
5.  len：获得数组，slice，map的长度以及channel缓冲区中元素的个数.

# go工程管理

1.  自己编写的package的名字一般要和文件夹一样(go允许二者不一样，但是为了避免混淆， 建议一样)，但是main
    package的文件夹一般不取main
2.  import指令是用来导入包的，导入的时候使用的是包所在文件夹的名字，而在代码中使
    用该包的时候使用的package的名字，所以为了避免混乱，应该让文件夹的名字和包名一
    致，可以使用相对导入，如果使用绝对导入，那么go就会去 `$GOPATH/src` 的目录下面 找
3.  go build 也要指定一个main package，main package可以使用绝对路径和相对路径，含 义和第一条一样,
    记住这里的main package是文件夹名。一般使用这样的命令来编译： `go build -o ./main
    main_package_dirname`
4.  也可以直接使用go run来运行代码。后面应该接main函数所在的那个文件的文件名。
5.  go get 可以用来从github或者bitbucket上下载代码，你只需要指定代码仓库的地址就 可以了，举个例子 `go get
    github.com/yuyang0/gt-go-sdk` 就会将代码下载到
    `$GOPATH/src/github.com/yuyang0/gt-go-sdk` 中, 所以你就可以导入这个包了

# reflect

这个是go语言的反射机制,它主要是用来获取interface下面值的type与value的, reflect包 有两个类型Type和Value.

1.  Type: 这个是用来代表值的类型的
2.  Value: 这个是用来代表值的.

# Cgo

## 注意事项(高能预警)

1.  import "C" 的上面不能有空行, 也就说这一行要和注释紧挨着.

2.  cgo不支持调用C语言中可变参数的函数, 所以你不能调用 `C.printf` 这类的函数

3.  cgo创建结构体时一般会有对齐,所以在初始化结构体时要注意, 必须显式的指定值对应的name.
    
    ``` go
    package main
    
    /*
       #include <stdlib.h>
       struct  {
           char *name;
           int age;
           int height;
       }person_t;
    */
    import "C"
    
    type Person C.person_t
    
    func main() {
      // error: too few values in struct initializer
      p1 := Person{C.CString("Giorgis"), 30, 6}
      // correct
      p2 := Person{name:C.CString("Giorgis"), age:30, height:6}
    }
    ```
    
    p1因为对齐的存在会编译报错.

## 类型转换

go可以很方便的和C语言交互，为了相互调用必须对数据类型进行转换，也就是在C的类型和
go的类型之间相互转换。整数字面值不需要使用C.int转换，所以在需要使用C.int的地方可
以直接使用1,2,3等，nil和NULL也不需要转换, 也就是说需要NULL的c函数可以直接给它传 递nil, 一个C函数如果返回NULL,
也可以让它与nil直接比较, 但是字符串字面值需要转换。

``` example
bool --> C.bool
char -->  C.char -->  byte
signed char -->  C.schar -->  int8
unsigned char -->  C.uchar -->  uint8
short int -->  C.short -->  int16
short unsigned int -->  C.ushort -->  uint16
int -->  C.int -->  int
unsigned int -->  C.uint -->  uint32
long int -->  C.long -->  int32 or int64
long unsigned int -->  C.ulong -->  uint32 or uint64
long long int -->  C.longlong -->  int64
long long unsigned int -->  C.ulonglong -->  uint64
float -->  C.float -->  float32
double -->  C.double -->  float64
void * -> unsafe.Pointer
```

**特别注意指针**, 因为golang类型系统的关系,指针转换基本都必须先转换成 `unsafe.Pointer`,
然后在将该指针转换成你想要的指针,要将go指针转换为c指针(\*int –\>
\*C.int) 需要这样的代码

``` go
var ig int = 1
(*C.int)(unsafe.Pointer(&ig))
```

对于C中需要void\*的场景,只需要将指针转换成 `unsafe.Pointer` 就好, 所以对于 `free` 应该这样调用

``` go
gs := "hello world"
cs := C.CString(gs)
C.free(unsafe.Pointer(cs))
```

## go中引用C

C中的名称都可以使用“C”这个“包”访问到

1.  类型：C.int, C.float, \*C.int 代表C中的int，foat以及int\*，
    
    ``` go
    package main
    
    /*
    #include <stdio.h>
    #include <stdint.h>
    int ic = 5;
    unsigned int uic = 7;
    int16_t is = 12345;
    */
    import "C"
    import (
      "fmt"
      "reflect"
      "unsafe"
    )
    
    func main() {
      var ig int = 10
    
      igc := int(C.ic)   // C int ==> Go int
    
      icg := C.int(ig)   // Go int ==> C int
    
      icp := (*C.int)(unsafe.Pointer(&ig)) // Go int pointer ==> C int pointer
    
      uigc := uint(C.uic)  // C uint ==> Go uint
    
      i64t := int16(C.is)  // C short ==> Go short
    }
    ```

2.  string对于string因为C的原因需要特殊处理, C string是不会被GC回收的，所以你要调 用 `defer
    C.free(unsafe.Pointer(x))` 来回收内存
    
    ``` go
    package main
    
    /*
        #include <stdlib.h>
        char* cstring = "C string example";
    */
    import "C"
    import (
      "fmt"
      "unsafe"
    )
    
    func main() {
    
      var gstring string = "Go string example"
    
      //Go to C String, Output: *C.char
      cs := C.CString(gstring)
      defer C.free(unsafe.Pointer(cs))
      fmt.Println(cs)
    
      //C to Go String, Output: string
      gs := C.GoString(C.cstring)
      fmt.Println(gs)
    
      //C string, length to Go string
      gs2 := C.GoStringN(C.cstring, (C.int)(len(gs)))
      fmt.Println(gs2)
    
    }
    ```

3.  函数：可以直接引用，C.printf, C.sqrt 等等, 只是你需要将参数转换成C的形式, 当
    调用C函数,你可以返回多个值,一个是C函数的返回值,一个根据errno封装的error对象,
    这在调用一些系统调用时非常有用.
    
    ``` go
    n, err := C.sqrt(-1)
    _, err := C.voidFunc()
    ```
    
    即便是C中的void函数也可以返回两个值, 当然这种情况下第一个值是没有意义的.

4.  struct 看代码
    
    ``` go
    package main
    
    /*
        #include <stdlib.h>
        struct Person {
            char *name;
            int age;
            int height;
            int weight;
        };
    */
    import "C"
    import "fmt"
    
    type p C.struct_Person
    
    func main() {
    
      person := p{C.CString("Giorgis"), 30, 6, 175}
      fmt.Println(person)
      fmt.Println(C.GoString(person.name))
      fmt.Println(person.age)
      fmt.Println(person.height)
      fmt.Println(person.weight)
    }
    ```

# Tips(best practice)

1.  处理错误时避免多重嵌套，先处理错误的情况
    
    ``` go
    if err != nil {
      // handle error
    }
    // do soamething
    ```

2.  尽量避免重复，可以定义一些一次性的类型来更好的组织代码

3.  使用type switch来处理type cast
    
    ``` go
    switch x := v.(type) {
    case string:
      fmt.Printf("%s", x)
    case int64:
      fmt.Print(x)
    }
    ```

4.  命名应该尽可能的短小，比如：
    
    ``` go
    package  db
    type DB struct {
    // some fields
    }
    
    // good
    func New() *DB {
    
    }
    //bad
    func NewDB() *DB {
    
    }
    ```
    
    因为有包名，所以就不需要在New后面加个DB的后缀了。

5.  大型的包最好组织成多个文件，这样可以使文档以及测试更模块化。

6.  在库以及API中间尽量不要使用并发，应该让调用者来决定是否需要在单独的goroutine中运行

7.  尽量使用channel或者一个带有channel的类型来在goroutine之间来通信。

8.  避免goroutine泄露，也就说goroutine永久的block，一直不退出，这实际上也是一种资 源泄露，通过buffered
    channel或者quit channel可以避免。quit channel是更通用的 做法

# 坑

## closure

先看一段错误代码

``` go
func main() {
  var wg sync.WaitGroup

  s := make([]int, 5)
  for i := 0; i < len(s); i++ {
    wg.Add(1)
    go func() {
      defer wg.Done()
      fmt.Print(s[i])
    }()
  }
  wg.Wait()
}
```

这段代码有很大可能出现index out of range的错误，对于for loop需要记住一点：

1.  `for exp {body}` 这样的表达式中, `exp` 中创建的变量在所有迭代中是共享的，也就
    是说是同一个变量，但是body中创建的变量是不共享的。你可以认为exp中创建的变量的
    作用域是for表达式所在的作用域，而不是body所在的作用域

所以上面的例子中i是共享的，所以对i的修改会传递到所有的goroutine中，因为goroutine
的启动会有一点延迟，所以等goroutine启动开始运行时i很可能已被修改成了5，自然就会
出现index out of range的错误，正确的做法:

1.  将i作为参数传递给goroutine的那个匿名函数。

2.  在body创建一个新的变量,eg
    
    ``` go
    func main() {
      var wg sync.WaitGroup
    
      s := make([]int, 5)
      for i := 0; i < len(s); i++ {
        i := i
        wg.Add(1)
        go func() {
          defer wg.Done()
          fmt.Print(s[i])
        }()
      }
      wg.Wait()
    }
    ```
    
    **匿名函数中使用上层作用域的变量要谨慎**.
