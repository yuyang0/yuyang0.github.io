---
title: cpp笔记
date: 2014-02-13 11:54:17
tags:
- cpp
- lang
categories:
- notes
---
# OOP

## 类

### 构造函数

默认构造函数只有当你没有定义构造函数的时候才会自动生成，一旦你一提供了构造函数， 那么你也应该提供默认的构造函数。

### tips

1.  如果所有的类成员都是public的，那么使用struct，否则使用class。
2.  仿函数(重载 operate())使用struct。

## 拷贝控制

### concepts

1.  直接初始化：
2.  拷贝初始化：一般使用等号的都是拷贝初始化。而且在下列场景也会有拷贝初始化
      - 将一个对象作为实参传递给非引用类型的形参
      - 从一个返回类型为非引用类型的函数返回一个对象
      - 标准库容器初始化或者调用insert或push时。
    拷贝初始化是使用拷贝构造函数实现的。

### 拷贝构造函数

``` example
class_name ( const class_name & ) {...}       (1)
class_name ( const class_name & ) = default;  (2)
class_name ( const class_name & ) = delete;   (3)
```

### 拷贝赋值运算符

### 移动构造函数

### 移动赋值运算符

### 析构函数

析构函数包含一个函数体和一个析构部分，首先执行函数体，接着就销毁所有的成员，成员 按初始化顺序的逆序销毁。 **析构部分是隐式的**.

不要在函数体中销毁成员, 因为这是析构部分的任务，有一个例外是指针成员，析构部分不
会对指针调用delete，所以你可能需要在析构函数体中delete。智能指针实际上是类，它有
析构函数，所以析构部分可以很好的处理他，在函数体中你不用操心。

调用析构函数的场景：

1.  变量离开作用域被销毁
2.  当一个对象被销毁时，其成员也会被销毁
3.  容器被销毁或者调用erase，clear，之类的函数删除元素时，被删除的元素都会被析构
4.  对指针进行delete，那么它指向的对象会被析构。

# STL

# Template

# Code Style

## 命名规范

1.  **类型命名** (类，结构体，枚举)使用驼峰命名,每个单词首字母均大写, 不包含下划线
2.  **变量名** 一律小写, 单词之间用下划线连接. 类的成员变量以下划线结尾, 但结构体的就不用
3.  **常量命名** 驼峰命名，在前面加 k 前缀， eg: `kDaysInAWeek`.
4.  **函数命名** 常规函数使用驼峰命名，getter/setter函数必须和类成员名字匹配， 如果 类有一个 `field_`
    的成员，那么getter和setter分别为： `field()` 和 `set_field`.
5.  namespace用小写字母命名, 并基于项目名称和目录结构

# 杂七杂八

## Mix C and C++

主要是通过 `extern "C"` 来实现，他主要是用来抑制C++的名称混淆(name mangle)的。

1.  c调用C++的库：常规的做法时是提供一个wrapper，你应该在C++中提供一个头文件 (c\_api.h),
    这个头文件要严格按照C的语法编写，同时在需要C调用的函数前应该添加上 `extern
    "C"`, 因为这个头文件会同时被C编译器和C++编译器调用，所以需要如下的代 码：
    
        #ifdef __cplusplus
        #define EXTERN_C extern "C"
        #endif

    有了头文件，你接着需要在一个C++文件中(eg:c\_api.cpp)实现头文件中定义的这些函数。 注意实现时是否加上 `extern
    "C"` 是可选的。

2.  C++中调用C库： 你只需要通过 `extern "C" void c_func()` 来声明就好，接着你就可 以调用 `c_func`
    了。

## Profile/Debug

### gperftools

1.  install
    
    ``` example
    sudo apt-get install google-perftools libgoogle-perftools-dev
    go get github.com/google/pprof
    ```

2.  cpu profile
    
    ``` example
    LD_PRELOAD=/usr/lib/libprofiler.so CPUPROFILE=cpu_profile ./prog
    ```

3.  heap profile
    
    ``` example
    LD_PRELOAD=/usr/lib/libtcmalloc.so HEAPPROFILE=heap_profile ./cbmc
    ```

4.  visualization
    
    ``` example
    pprof --gv cpu_profile cbmc
    ```
