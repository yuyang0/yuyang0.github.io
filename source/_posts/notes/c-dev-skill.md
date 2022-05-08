---
title: C备忘
date: 2014-01-10 11:54:17
tags:
- c
- lang
categories:
- notes
---

# Valgrind

一个内存泄露的检查工具。

# systemtap

## install

安装debug的符号表, 先使用 `cat /proc/version_signature` 获得内核版本, 不要使 用 `uname -r`,
因为它有时候获得的内核版本不准, 然后去[这里](http://ddebs.ubuntu.com/pool/main/l/linux/)
下载相应的包, 比如 linux mint17内核版本是 Ubuntu 3.13.0-24.47-generic 3.13.9,
而且是64位机器,
那么就要下载linux-image-3.13.0-24-generic-dbgsym\_3.13.0-24.47\_amd64.ddeb这个
包, 接着使用 `sudo dpkg -i xx` 安装, 然后 `sudo apt-get install systemtap`

## 脚本语言相关

systemtap脚本的基本单元是这样的

``` example
probe   event {statements}
```

基本规则就是一旦触发了event,那么就执行后面的statements, 脚本中可以有多条 probe,
statements又叫handler或者body.

### event

event主要有这几类:

1.  syscall.XXX: 监听系统调用, 后面加上return就是监听系统调用返回,可以使用通 配符.
    
    ``` example
    probe syscall.open { }
    probe syscall.open.return { }
    probe syscall.* {}
    ```

2.  vfs.XXX: 监听文件系统的事件

3.  kernel.function("function"): 监听内核函数, 比如
    
    ``` example
    probe kernel.function("sys_open") { }
    probe kernel.function("sys_open").return { }
    
    probe kernel.function("*@net/socket.c") { }
    probe kernel.function("*@net/socket.c").return { }
    ```
    
    后两行会监听net中socket.c中所有函数的调用与返回.

4.  kernel.trace("tracepoint"):

5.  module("module").function("function"):　监听指定模块的指定函数
    
    ``` example
    probe module("ext3").function("*") { }
    probe module("ext3").function("*").return { }
    ```

6.  begin: systemtap启动时会触发, 如:
    
    ``` example
    probe begin {
        printf("hello world")
        exit()
    }
    ```
    
    启动时执行一次,打印hello world就退出

7.  end: 结束时执行一次

8.  time.xxx(): 定时器, 时间到了就会运行, xxx可以是s, ms, us, ns, hz, jiffies
    
    ``` example
    probe timer.s(4) {
      printf("hello world\n")
    }
    ```
    
    每4秒打印hello world.

### handler

# 链接相关

1.  ldd可以用来检查运行时的动态链接库错误,它可以提示你哪些库没有找到.

# 奇淫技巧

## 0长度数组(GNU C)

看代码

``` c
struct str{
    int len;
    char s[0];  /* c99应该这样 char s[] */
};
int len = 10;
struct str *p = malloc(sizeof(struct str) + (len+1) * sizeof(char));
p->len = len;
```

s实际上是不占空间的, 它可以h看作是一个占位符, p-\>s 会自动指向紧挨着结构体的 内存地址, 如果使用指针,不仅会多占用4字节的空间,
而且必须手动的给该指针赋值.注 意该特性是GNU C的扩展,C99可以用可变长度的数组,但是这有一些限制,比如不能对结
构体调用sizeof(因为长度无法确定).

## 原子操作

``` c
struct aa {
    int lock;
    /* other field */
};
struct aa S;
static inline void
LOCK() {
    while (__sync_lock_test_and_set(&(S.lock),1)) {}
}

static inline void
UNLOCK() {
    __sync_lock_release(&(S.lock));
}
```

上面的代码实现了一个简单的同步机制, 利用GNU C的原子操作.这些原子操作只能对 int, long, long
long以及这些类型的无符号版本使用,

1.  原型: `type __sync_lock_test_and_set (type *ptr, type value, ...)`, 先获
    得\*ptr当前值,然后将ptr指向的值设为value
2.  原型: `void __sync_lock_release (type *ptr, ...)`, 将\*ptr设为0.

完整的列表在[这里](https://gcc.gnu.org/onlinedocs/gcc-4.1.2/gcc/Atomic-Builtins.html)

# Linux相关

## EPOLL
1. 水平触发(LT): 如果内核通知应用层某一个文件描述符已经就绪，而如果应用层此后
    一直没有完整的处理该描述符（没有读取完相应的数据或者没有写入任何数据），那
    么内核会不断地通知应用层该文件描述符已经就绪。这就是所谓的水平触发：只要条
    件满足，那内核就会触发一个事件（只要文件描述符对应的数据没有被读取或者写入，那内核就不断地通知你）**该模式用的多**
2. 边沿触发(ET): 内核只是在文件描述符的状态发生变换的时候才进行通知。这就意味着在大多数情况下，当内核通知某个读描述符就绪 后，除非该读描述符内部缓冲区的所有数据已经完全被读取从而使得就绪状态发生了变化 否则内核不会发出任何新的通知，会永远沉默下去

## hugepages

``` example
echo "default_hugepagesz=1G hugepagesz=1G hugepages=4" > /etc/default/grub
sudo update-grub
```

# Makefile相关

make运行makefile分为两步

1.  扫描整个makefile，把所有的include的makefile都包含进来，进行必要的变量替换，接着生成依赖图
2.  根据依赖图来执行动作

## 变量

makefile中的变量和C里面的宏很类似，就是直接原样替换

1.  定义: 两种定义方法，一种可以使用后面定义的变量（=），一种不可以(:=)
    
    ``` example
    var = $(AA)aaaa
    var := $(AA) bb
    ```

2.  变量的优先级是：
    
      - 命令行指定的变量优先级最高: 它会使makefile内部对同一变量的赋值无效，也就
        是说只要命令行指定了，那么就以命令行为准，当然makefile中的变量使用了
        override关键字，那么makefile中的赋值会覆盖命令行的定义。
      - makefile内部的赋值创建的变量其次
      - 运行make时的环境变量：环境变量在命令行没有指定该变量时会成为该变量的默认
        值,可以通过export，unexport来为makefile中运行的shell命令设置或者清除环境
        变量。

# 基本概念

1.  Reactor： 实际就是事件循环，libev，tornado都是这一类
2.  Actor：万物都是actor，可以理解为一系列的轻量级进程，他们只能通过消息来通信，代表语言是erlang
3.  CSP：一系列轻量级的进程，进程间通过channel来通信，等待channel的消息时，进程会 挂起，代表语言是go

# GDB

## 启动GDB

``` example
gdb prog                        # 1
gdb prog core                   # 2
gdb prog 1234                   # 3 (二者等价)
gdb -p 1234
gdb --args gcc -O2 -c foo.c     # 4
```

1.  直接启动gdb调试prog
2.  调试prog程序产生的core文件
3.  调试一个正在运行的进程, 使用 `-p` 参数可以让你忽略prog。
4.  使用–args将参数传递给要调试的程序

## 常用命令

1.  shell xxx：　执行shell命令， 比如 `shell ls` 就会显示当前目录的文件
2.  help： 显示帮助
3.  cd/pwd: 改变/打印当前的工作目录。
4.  run(r)： 直接运行程序直到遇到第一个断点，如果没有断点，那么就运行到程序结 束
5.  start: 和run的区别是该命令会停在main的入口处。。
6.  dir： 添加一个目录到gdb的source path， 该路径是用来查找源文件的，比如调试
    python时，如果你要进入python解释器的源码，那么你就要自己获得一份源码，同时
    使用dir命令将源码位置添加到source path。

## 常见问题解决

### 调试python

1.  获得一份python源码
2.  使用如下命令 `gdb -ex r --args python test.py` 运行脚本
3.  使用dir命令将源码位置添加到source path： `dir /path/to/source/Python` 注
    意我们添加的目录是源码顶层目录下的python子目录。
