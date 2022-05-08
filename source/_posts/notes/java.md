---
title: java笔记
date: 2014-02-16 11:54:17
tags:
- java
- lang
categories:
- notes
---
# tools

## run program

1.  in Terminal
    
    ``` example
    javac Welcome.java
    java welcome
    ```

# OOP

OOP编程范式最核心的三大概念：继承，封装，多态。

## package

建议使用域名的逆序来命名包, 比如有一个域名: example.com, 那么你就可以使用

``` java
package com.example.testcode
```

来申明一个文件属于 com.example.testcode包, 同时该文件要放入
com/example/testcode/目录中.使用IDE可以很轻松的做到这一点.

## 继承

java的所有继承都是公有继承,使用 `extends` 关键字.

``` java
class Employee{
        public Employee(String name, double salary){
                this.name = name;
                this.salary = salary;
        }
        public String getName(){
                return this.name;
        }
        public double getSalary(){
                return this.salary;
        }
        public void raiseSalary(double byPercent){
                double raise = this.salary * byPercent /100;
                this.salary += raise;
        }

        private String name;
        private double salary;
}

class Manager extends Employee{
        public Manager(String name, double salary){
                super(name, salary); // super调用父类的构造方法
                this.bonus = 0;
        }
        public double getSalary(){
                double baseSalary = super.getSalary(); // super调用父类的普通方法
                return baseSalary + this.bonus;
        }
        public void setBonus(double bonus){
                this.bonus = bonus;
        }
        private double bonus;
}

```

在类的方法中可以直接访问该类的所有实例的私有属性,所以在Employee的equals方法 中可以直接使用
`other.name`,但是在子类的方法中不能直接访问父类的私有成员,所 以上例中 Manager 类中的
`etSalary` 方法不能直接 `this.salary` 而必须用父类 的公有方法即 `super.getSalary()`,
Manager 中的构造函数也是同样的道理.super 有两个作用:一个是调用父类的方法,一个是调用父类的构造方法

### 类型转换

1.  子类变量可以直接赋给父类变量,这样父类变量调用方法时会使用动态绑定,比如 如下代码:
    
    ``` java
    Employee e = new Manager(...);
    e.getSalary();
    ```
    
    上面的getSalary调用的是 Manager的方法,而不是Employee的方法.这实际就是所 谓的 **多态**,
    也就是说相同的编译时类型调用相同的方法可能呈现出不同的行为.这
    里需要注意的是e只能调用Employee类拥有的方法，虽然这里的e是一个Manager实
    例，但是它不能调用 `setBonus` 方法，如果你调用那么编译器会报错，这好理解， 因为编译时，e是一个Employee类型的变量.
    而Employee类型很显然没有 `setBonus` 方法。

2.  父类变量要赋给子类变量,则必须使用强制转化, 比如类 Manager 是 Employee 的子类, 那么:
    
    ``` java
    Employee e = new Manager(...);
    if(e instanceof Manager){
        Manager m = (Manager) e;
    }
    ```
    
    一般情况下使用 instanceof 来检查下会更保险.

### Object

Object是所有类的超类,该类有有一些方法建议每一个类都重载:

1.  equals方法: 用来检测一个对象是否等于另一个对象,下面是一个推荐的实现方 式:
    
    ``` java
    class Employee{
        ....
        public bool equals(Object otherObject){
            if(this == otherObject) return true;
            if(otherObject == null) return false;
            if(getClass() != otherObject.getClass()) return false;
    
            Employee other = (Employee) otherObject;
            return name.equals(other.name)
                && salary == other.salary;
        }
    }
    class Manager extends Employee{
        ...
        public bool equals(Object otherObject){
            if (! super.equals(otherObject)) return false;
    
            Manager other = (Manager) otherObject;
            return this.bonus == other.bonus;
        }
    }
    ```
    
    基本上所有的 `equals` 都可以用这种模式来定义, 但是对于子类, 比如manager
    类就只需要调用父类的equals方法,然后在加入一些特定于Manager类的检测就好

2.  toString方法: 用来返回类实例的字符串表示.该方法调试时非常方便

3.  hashCode方法: 获取一个类实例的散列值

## 抽象类

抽象类不能实例化,它主要是用来被其它类继承的,实际上起的是一个占位的角色,大多
数时候,抽象类只会包含一系列的抽象方法,但是抽象类是可以包含实例域与普通的类
方法的,下面的代码展示了抽象类的用法.

``` java
abstract class Person{
    public Person(String name){
        this.name = name;
    }

    public abstract String getDescription();

    public String getName(){
        return this.name;
    }
    private String name;
}
```

实际上一般抽象类主要用来作为创建其它类的模板。

## 反射(Reflection)

虚拟机为每一个类型都维护了一个Class对象, 要获得Class对象有以下几种方法:

1.  如果T是任意的java类型(必须是类型不能是类实例),那么 T.class 就是Class对象. 比如
    Employee.class就是上例中 Employee的Class对象, int.class就是int的 Class对象.

2.  使用 getClass实例方法, 如果e 是一个Employee实例, 那么 e.getClass()就会返
    回Employee类型的Class对象.

3.  如果知道类的字符串表示, 那么可以使用 forName来获得Class 对象, 比如:
    
    ``` java
    Class cl = Class.forName("java.util.Date");
    ```

获得Class对象后, 就可以来获取类型的信息了.

1.  newInstance 获取一个新的实例,注意返回的是Object类型, 所以你需要进行类型 转换
    
    ``` java
    Class cl = Class.forName("java.util.Date");
    Object m = cl.newInstance();
    Date d = (Date) m;
    ```

## 接口(interface)

1.  定义
    
    ``` java
    public interface Comparable<T>{
        int CompareTo(T otherObject);
    }
    ```
    
    interface中的方法自动为public.

2.  实现接口
    
    ``` java
    class Employee implements Comparable<Employee>{
        public int compareTo(Employee other){
            ...
        }
    }
    ```
    
    如果想使用Arrays中的sort方法,那么应该实现 `Comparable` 接口.

3.  接口的特性:接口不可以被实例化,但是可以申明变量,而且可以将实现了该接口的 类的实例赋给该变量, 而且可以使用 `instanceof`
    来测试一个实例所属的类是否 实现了该接口.
    
    ``` java
    x = new Comparable();            // 错误语法
    
    Comparable x;
    Comparable y = new Employee(...)           // Employee类实现了Comparable接口
    ```

4.  接口类型的变量只能调用接口中定义的方法，这和多态的情况是一样的，因为只有 这样才能通过编译时检查.

# 杂七杂八

1.  java提供了八种基本类型(byte,short,int,long,char,boolean,float,double)这些类型
    不是对象，但是java提供了包装对象，并且在适当的地方会自动的装箱和拆箱
    (boxing,unboxing).java中所有的对象类型都是引用类型
2.  \==与equals： 前者在基本类型为数值并且值相等时有效，对引用类型只有两个引用指向 同一个对象时才相等。

# exception(异常)

![](static/img/exceptions.png)

所有的异常都是从 Throwable继承而来的,而且在下一层就立即分为2类: Error与 Exception,
Error是java运行时系统的内部错误, 这类错误程序员无能为力,只能让程
序尽可能安全的终止.Exception又分为两类: RuntimeException 与
IOException, RuntimeException主要有以下几种情况:

1.  错误的类型转换 (可以通过转换前使用instanceof检测类型来避免)
2.  数组访问越界
3.  访问空指针

这些异常都是程序应该避免的,是程序设计者的失误导致的, 发生了这类异常那就意味
着程序不正确,应该花时间来修复而不是寄希望于catch来处理这些异常.

IOException是所有不属于RuntimeException的异常,这类异常应该通过try catch 这类 设施进行处理.
一个类方法如果要抛出异常,那么必须在定义的行显式的声明,比如下面 的例子:

``` java
class MyAnimation{
    ...
    public Image loadImage(String s) throws EOFException, MalformatURLException{
        ...
    }
}
```

loadImage有可能抛出两个异常,如果一个方法没有声明 throws语句,那么就不能抛出异
常.如果是重载父类的方法,那么该方法的throws申明必须和父类的方法一模一样,异常
处理的基本代码是这样的:

``` java
try{
    ...
}
catch(MakformatURLException e){
    ...
}
catch(IOException e){
    ...
}
finally{
    ...
}
```

多个catch可以捕获多种类型的异常, finally块的语句是无论如何都会执行的,所以可 以用来清理资源

## 异常使用的建议

1.  如果一个方法中调用了一个能够抛出异常的方法,那么这时候有两个选择,一是如果 目前能够处理该异常, 那么就使用try
    catch来捕获该异常.二是将该异常传递给该 方法的调用者, 那么你就应该在方法头上添加对应的
    throws语句.如果是在重载父 类的方法,而父类的方法上没有该异常的 throws语句,那么你就必须在本方法中使 用 try
    catch来捕获这个异常.
2.  异常处理不能代替简单的测试,测试更高效
3.  不要过分细分异常,比如不要一条语句一个 try catch,只要将逻辑上的多个语句包 进一个try catch就好
4.  尽量使用异常来代替一些特殊的返回值,所以stack.pop()不要返回一个null,而应
    该生成一个EmptyStackException异常.

# 日志

1.  获得logger, 主要是使用 Logger类的 getLogger工厂方法, 该方法只有在logger 不存在时才创建
2.  设置logger的level, 使用logger的setLevel方法,level有以下几种(优先级由高到 低):
      - SEVERE
      - WARNING
      - INFO (默认)
      - CONFIG
      - FINE
      - FINER
    上面的都是Level类的静态常量,logging只会记录优先级高于等于当前设置的level 的日志,比如默认的日志级别是 INFO,
    那么就只会记录 SEVERE, WARNING, INFO三 个级别的日志,还有两个特殊的常量:Level.ON,
    Level.OFF,前者会记录所有级别的 日志,后者不记录任何级别的日志
3.  Handler:确定日志都存储到什么地方,一个logger实例可以有多个handler,每个
    handler也有自己的level,只有日志级别高于等于handler的level时,handler才会处
    理
      - ConsoleHandler: 直接控制台打印
      - FileHandler: 文件存储
      - SocketHandler: 写入socket, 比如日志服务器
4.  Filter:
5.  Format: 默认logging可以将日志记录为文本与 xml, 如果你想记录为其它格式,可 以自定义 Format类, 同时
    使用handler的 setFormator来安装.
6.  记录日志,每一个level都有一个对应的方法来记录相应级别的日志,比如 `logger.severe`,
    `logger.warning` 等等, 同时还有一个 `logger.log` 方法,这 个方法需要你显式的指定日志级别.

<!-- end list -->

``` java
Logger logger = Logger.getLogger("com.mycompany.myapp");
logger.setLevel(Level.FINE);
logger.setUseParentHandler(false); // 防止日志送入 父logger,因为这样会重复
Handler hd = new ConsoleHandler();
hd.setLevel(Level.Fine);
logger.addHandler(hd);
```

## 使用建议

1.  将logger 命名为与主应用程序包一样的名字.
    
    ``` java
    Logger logger = Logger.getLogger("com.mycompany.myproj");
    ```
    
    同时为了方便可以将logger添加进类的静态域中
    
    ``` java
    class MyClass{
        ...
        private static final Logger = Logger.getLogger("com.mycompany.myproj");
    }
    ```

2.  
# java标准库

## java.lang(默认导入)

### java.lang.String(字符串)

字符串是不可变的.所以修改会返回一个新的字符串

  -   - 可以拼接字符串

  - equals: 检测两个字符串是否相等, **不能用==**.

  - String toLowerCase()

  - String toUpperCase()

  - String trim()

  - int length()

  - boolean startsWith(String prefix)

  - String substring(int start \[, int end\]) 不包含end指向的字符

  - String replace

  - format: 静态方法

<!-- end list -->

``` java
String s1 = "aaaa";
String s2 = s1 + "bbbb";        // s2 = "aaaabbbb"
int len = s2.length();    // len = 8
String newStr = s2.substring(0, 3); // newStr = "aaa"
```

### java.lang.StringBuilder

该类表示的字符串可变,主要是为了效率考虑

``` java
StringBuider builder = new StringBuider();
builder.append("hello");
builder.append(' ');
String completeString = builder.toString();
```

### java.lang.Math

各种数学函数以及变量, 都是静态函数以及变量

  - Math.PI: 圆周率
  - Math.E: 自然对数
  - Math.sin, Math.cos, Math.floor etc

### java.lang.System

## java.util

### java.util.Date(Time)

### java.util.GregorianCalendar(日历)

### java.util.ArrayList

## java集合

java的集合大体上分为Set，List，Map三类。集合类型的元素必须是引用类型，而不能是基 本类型，但是一般java会自动装箱
