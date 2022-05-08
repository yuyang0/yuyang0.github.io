---
title: gevent笔记与源码分析
date: 2018-02-11 11:54:17
tags:
- gevent
categories:
- articles
---

# 基本概念

## coroutine

Coroutine 也就是 corporate routine，中文名就是协程，从它的英文可以看出是协同 的例程的意思,
实际上这个概念和进程与线程有相似之处, 因为linux线程就是所谓的
轻量级进程,所以我们来比较一下进程与协程的异同:

  - **相同点**:二者都是可以看做是一种执行流, 该执行流可以挂起,并且在将来又可以在 你挂起的地方恢复执行,
    这实际上都可以看做是continuation, 我们来看看当我们挂
    起一个执行流时我们要保存的东西
    1.  **栈**, 因为如果你不保存栈,那么局部变量你就无法恢复,同时函数的调用链你也无 法恢复,
    2.  **寄存器的状态**: 这好理解, 比如说EIP,如果你不保存,那么你恢复执行流就不知道 到底执行哪一条指令,
        在比如说ESP,EBP, 如果你不保存,那么你即便有完整的栈 你也不知道怎么用.
    这二者实际就是所谓的上下文,也可以说是continuation. 在执行流切换时必须保存 这两个东西, 内核调度进程时也是一回事.
  - **不同点**:
    1.  执行流的调度者不同, 进程是内核调度, 而协程是在用户态调度, 也就是说进程
        的上下文是在内核态保存恢复的,而协程是在用户态保存恢复的.
        很显然用户态的 代价更低
    2.  进程会被抢占,而协程不会,也就是说协程如果不主动让出CPU,那么其他的协程是不
        可能得到执行机会,这实际和早期的操作系统类似,比如DOS,
        它有一个yield原语, 一个进程调用yield,那么它就会让出CPU, 其他的进程也就有机会执行了, 如果一
        个进程进入了死循环,那么整个系统也就挂起了,永远无法运行其他的进程了, 但
        对协程而言,这不是问题
    3.  对内存的占用不同,实际上协程可以只需要4K的栈就够了, 而进程占用的内存要大 的多.
    4.  从操作系统的角度讲, 多协程的程序是单线程,单进程的

# gevent背景知识

gevent用到了了libev以及greenlet还有cares,下面简单的介绍这几个库.

## greenlet

实际是一个协程库(官方叫micro-thread), 它只提供协程本身,要在协程间切 换调度必须你在程序中手动来进行,直接上代码:

``` python
def test1(x, y):
    z = gr2.switch(x+y)
    print z

def test2(u):
    print u
    gr1.switch(42)

gr1 = greenlet(test1)
gr2 = greenlet(test2)
gr1.switch("hello", " world")
```

gr1, gr2以及运行该代码的解释器本身都是协程, 协程对象的switch方法用 来切换,比如 `gr2.switch` 就是切换到gr2.

### api

1.  greenlet(run=None, parent=None): 创建一个greenlet实例.
2.  greenlet.getcurrent:

实例方法与属性

1.  gr.parent:每一个协程都有一个父协程,当前协程结束后会回到父协程中执行,该 属性默认是创建该协程的协程.
2.  gr.run: 该属性是协程实际运行的代码. run方法结束了,那么该协程也就结束了.
3.  gr.switch(\*args, \*\*kwargs): 切换到gr协程.
4.  gr.throw(): 切换到gr协程,接着抛出一个异常.

## libev

和libevent类似, libev是一个事件循环库,他可以监听各个file descriptor, 一旦发现 就绪就调用对应的回调函数,
gevent内部就是使用libev来监听socket的.为了后续理解 方便有必要对libev进行简单的了解.

1.  watcher: 实际上是用来封装各种类型的事件的,不同类型的事件会有不同类型的 watcher, 比如 `ev_io`,
    `ev_timer`, 该结构一般会有一个回调函数,当事件触发 使就会调用回调函数.
    watcher会有两种函数(注意TYPE代表watcher类型,可以是 io, timer,
    signal等等):
      - ev\_TYPE\_init: 对watcher对象进行初始化, 对IO而言该函数是 `ev_io_init`,
        对timer而言,该函数是 `ev_timer_init`.
      - ev\_TYPE\_set: 与init系列函数的区别是该函数一般不设置callback
      - ev\_TYPE\_start: 将watcher注册到事件循环中,这样就可以监听事件了.
2.  loop: 事件循环

来看看示例代码:

``` c
// a single header file is required
#include <ev.h>

#include <stdio.h> // for puts

// every watcher type has its own typedef'd struct
// with the name ev_TYPE
ev_io stdin_watcher;
ev_timer timeout_watcher;

// all watcher callbacks have a similar signature
// this callback is called when data is readable on stdin
static void
stdin_cb (EV_P_ ev_io *w, int revents)
{
    puts ("stdin ready");
    // for one-shot events, one must manually stop the watcher
    // with its corresponding stop function.
    ev_io_stop (EV_A_ w);

    // this causes all nested ev_run's to stop iterating
    ev_break (EV_A_ EVBREAK_ALL);
}

// another callback, this time for a time-out
static void
timeout_cb (EV_P_ ev_timer *w, int revents)
{
    puts ("timeout");
    // this causes the innermost ev_run to stop iterating
    ev_break (EV_A_ EVBREAK_ONE);
}

int
main (void)
{
    // use the default event loop unless you have special needs
    struct ev_loop *loop = EV_DEFAULT;

    // initialise an io watcher, then start it
    // this one will watch for stdin to become readable
    ev_io_init (&stdin_watcher, stdin_cb, /*STDIN_FILENO*/ 0, EV_READ);
    ev_io_start (loop, &stdin_watcher);

    // initialise a timer watcher, then start it
    // simple non-repeating 5.5 second timeout
    ev_timer_init (&timeout_watcher, timeout_cb, 5.5, 0.);
    ev_timer_start (loop, &timeout_watcher);

    // now wait for events to arrive
    ev_run (loop, 0);

    // break was called, so exit
    return 0;
}
```

获得loop对象, 创建一个io watcher,一个timer watcher, 分别初始化(调用init函
数),然后调用start注册回调函数到事件循环中, 接着调用ev\_run启动事件循环.

# gevent源码分析

开始之前先申明: **本文分析的是gevent1.0**.

## core

core.ppyx文件实际上是用Cython写的代码，在Makefile中有这样一行代码:

``` example
gevent/gevent.core.c: gevent/core.ppyx gevent/libev.pxd
        $(PYTHON) util/cythonpp.py -o gevent.core.c gevent/core.ppyx
        echo                          >> gevent.core.c
        echo '#include "callbacks.c"' >> gevent.core.c
        mv gevent.core.* gevent/

```

上面的代码告诉我们core.ppyx会先转换为gevent.core.c这个C语言文件,然后在编译成 动态链接库，它的语法基本能看懂,
这个模块主要是实现loop这个类, hub对象中的
loop就是这个类的一个对象(注意名字虽然相同但一个是类,一个对象,不要弄混),这个
类将libev的事件循环机制封装了起来,我们先熟悉下这个类提供的API,熟悉这些API对
使用gevent很有帮助,我假设lp是loop类的对象(在gevent中就是get\_hub().loop)

``` example
def io(self, int fd, int events, ref=True, priority=None):
    return io(self, fd, events, ref, priority)

def timer(self, double after, double repeat=0.0, ref=True, priority=None):
    return timer(self, after, repeat, ref, priority)

def signal(self, int signum, ref=True, priority=None):
    return signal(self, signum, ref, priority)

def idle(self, ref=True, priority=None):
    return idle(self, ref, priority)

def prepare(self, ref=True, priority=None):
    return prepare(self, ref, priority)

def fork(self, ref=True, priority=None):
    return fork(self, ref, priority)

def async(self, ref=True, priority=None):
    return async(self, ref, priority)
```

注意上面是Cython, 上面的一系列方法实际是libev中watcher的等价物.比如你调用 `lp.io(fd, 1)`,
就创建了一个监听fd的read事件的watcher对象,至于其它的api都是 类似,
每一个watcher对象都有一个 `start` 方法, 该方法接受一个回调函数以及一系 列传递给回调函数的参数,
调用该方法就会将watcher对象注册到libev的事件循环上, 看下面的示例:

``` python
read_watcher = lp.io(fd, 1)
read_watcher.start(cb, args)
```

运行上面的两行代码,那么当fd上读就绪时,那么就会调用cb函数,并且会把args传递给
cb函数.在gevent中回调函数一般是协程的switch方法,
这样一旦调用,那么就切换到 另一个协程中去执行.

### core源码分析

这一节来分析core.ppyx的源码, 在一次提醒你注意,代码是Cython, 我对Cython也不 太熟,但是代码大致能看懂.
同时要明白该模块是对libev的封装, libev中loop,
watcher,callback在该模块中都有对应物, 所以你要注意当提到loop时,我们到底是
该模块的loop还是libev的loop.

1.  先看callback
    
    ``` example
    cdef public class callback [object PyGeventCallbackObject, type PyGeventCallback_Type]:
        cdef public object callback
        cdef public tuple args
    
        def __init__(self, callback, args):
            self.callback = callback
            self.args = args
    
        def stop(self):
            self.callback = None
            self.args = None
    ```
    
    实际上就是把回调函数以及要提供给回调函数的参数封装了起来.

2.  loop: 事件循环的封装.
    
    ``` example
    cdef public class loop [object PyGeventLoopObject, type PyGeventLoop_Type]:
        cdef libev.ev_loop* _ptr
        cdef public object error_handler
        cdef libev.ev_prepare _prepare
        cdef public list _callbacks
        cdef libev.ev_timer _timer0
    
        def __init__(self, object flags=None, object default=None, size_t ptr=0):
            cdef unsigned int c_flags
            cdef object old_handler = None
            libev.ev_prepare_init(&self._prepare, <void*>gevent_run_callbacks)
            libev.ev_timer_init(&self._timer0, <void*>gevent_noop, 0.0, 0.0)
            if ptr:
                self._ptr = <libev.ev_loop*>ptr
            else:
                ......
    
            self._callbacks = []
    ```
    
    1.  \_ptr: libev的一个ev\_loop对象.
    2.  \_prepare: libev中的prepare watcher,该watcher注册的回调函数会在事件循环进 入阻塞时调用,
        从代码中可以看到注册的回调函数是 `gevent_run_callbacks`, 该函数会运行 `_callbacks`
        列表中的每一个callback实例.
    3.  \_callbacks: 一个列表,实际上当你使用gevent的spawn创建协程时, spawn会在
        该列表中插入一个callback实例, 该实例的回调函数实际就是你创建的
        greenlet的switch方法, 这样当 `_prepare` watcher就绪时,新的协程就有了 启动的机会.

3.  watcher: 这是libev的watcher对象的封装,作为例子,我只分析io这一个例 子,timer,signal等等都是相似的,
    为了方便我使用cwatcher来指代libev中的 watcher.
    
    ``` example
    
    #define WATCHER_BASE(TYPE)                                            \
        cdef public loop loop                                             \
        cdef object _callback                                             \
        cdef public tuple args                                            \
        cdef readonly int _flags                                          \
        cdef libev.ev_##TYPE _watcher                                     \
    
    cdef public class io(watcher) [object PyGeventIOObject, type PyGeventIO_Type]:
    
        WATCHER_BASE(io)
    
        def start(self, object callback, *args, pass_events=False):
            CHECK_LOOP2(self.loop)
            if callback is None:
                raise TypeError('callback must be callable, not None')
            self.callback = callback
            if pass_events:
                self.args = (GEVENT_CORE_EVENTS, ) + args
            else:
                self.args = args
            LIBEV_UNREF
            libev.ev_io_start(self.loop._ptr, &self._watcher)
    
        def __init__(self, loop loop, int fd, int events, ref=True, priority=None):
            if fd < 0:
                raise ValueError('fd must be non-negative: %r' % fd)
            if events & ~(libev.EV__IOFDSET | libev.EV_READ | libev.EV_WRITE):
                raise ValueError('illegal event mask: %r' % events)
            libev.ev_io_init(&self._watcher, <void *>gevent_callback_io, fd, events)
            self.loop = loop
            if ref:
                self._flags = 0
            else:
                self._flags = 4
            if priority is not None:
                libev.ev_set_priority(&self._watcher, priority)
    ```
    
    1.  WATCH\_BASE, 它实际上定义了一系列的属性:
        
          - loop: 实际是上面分析的loop类的一个实例
          - \_watcher: cwatcher对象,也就是一个libev的ev\_io对象.
          - callback: 回调函数, 注意该回调函数是由上层传递进来,它不是由libev直接
            调用,而是由libev的回调函数调用,具体到本例就是被
            `gevent_callback_io` 调用.
          - args: 一个元组,传递给回调函数的参数
    
    2.  <span class="underline"><span class="underline">init</span></span>:
        该函数会设置loop属性,同时初始化libev的io watcher对象 `_watcher` (主要做两件事:
        指定事件类型,指定回调函数), 注意它的回调函数 是 `gevent_callback_io`
    
    3.  start: 该函数中 会设置回调函数以及参数, 这里设置的回调函数是上层传入的, 不要和libev的回调函数混淆, 同时调用
        `ev_io_start` 将该watcher注册到 libev的事件循环中. 为了弄明白libev事件循环的过程,我接下来分析
        `gevent_callback_io`.
    
    4.  gevent\_callback\_io
        
        ``` c
        #define GET_OBJECT(PY_TYPE, EV_PTR, MEMBER)                             \
            ((struct PY_TYPE *)(((char *)EV_PTR) - offsetof(struct PY_TYPE, MEMBER)))
        
        static void gevent_callback_io(struct ev_loop *_loop, void *c_watcher, int revents) {
            struct PyGeventIOObject* watcher = GET_OBJECT(PyGeventIOObject, c_watcher, _watcher);
            gevent_callback(watcher->loop, watcher->_callback, watcher->args, (PyObject*)watcher, c_watcher, revents);
        }
        ```
        
        GET\_OBJECT的作用是通过结构体中某一个域的指针来获得整个结构体的指针. 如果
        你熟悉linux内核就会发现它和container\_of的功能很相似.
        所以这里实际就是根 据cwatcher对象\_watcher来获得watcher的指针, 接着就调用
        `gevent_callback`.
        
        ``` c
        static void gevent_callback(struct PyGeventLoopObject* loop, PyObject* callback,
                                    PyObject* args, PyObject* watcher, void *c_watcher,
                                    int revents) {
            ......
            result = PyObject_Call(callback, args, NULL);
            ......
        }
        ```
        
        所以该函数就调用了上层传入的callback.

### core的api总结

假设Loop代表类, loop代表实例

1.  loop.run: 启动事件循环
2.  loop.run\_callback(fun, \*args): 将fun注册给loop的\_prepare watcher,这样
    fun就会在事件循环要阻塞时运行, spawn以及rawlink都会使用该方法.
3.  loop.io: 创建一个IO watcher实例, 调用该实例的start方法来注册回调函数,同 时将该watcher放入事件循环.
4.  loop.timer: 创建Timer Watcher对象
5.  loop.signal: 创建signal Watcher对象
6.  loop.idle:
7.  loop.prepare:
8.  loop.fork:

**注意使用io,timer, signal** 等方法创建watcher对象后, 必须调用该对象start方法
才能将watcher注册到事件循环中

## HUB

这实际上是greenlet的子类,所以它的每一个实例实际上就代表一个协程,这个类创建的 协程是专门用来运行事件循环的.

``` python
class Hub(greenlet):
    ...

    NOT_ERROR = (GreenletExit, SystemExit)
    loop_class = config('gevent.core.loop', 'GEVENT_LOOP')
    ...
    backend = config(None, 'GEVENT_BACKEND')
    ...

    def __init__(self, loop=None, default=None):
        greenlet.__init__(self)
        if hasattr(loop, 'run'):
           ...
        else:
            ...
            loop_class = _import(self.loop_class)
            if loop is None:
                loop = self.backend
            self.loop = loop_class(flags=loop, default=default)
        ...
```

创建一个hub实例, 这个实例最重要的就是loop属性,这个实际就是core模块的loop类的 实例,也就是说是libev的事件循环的封装.

``` python
def run(self):
    assert self is getcurrent(), 'Do not call Hub.run() directly'
    while True:
        loop = self.loop
        loop.error_handler = self
        try:
            loop.run()
        finally:
            loop.error_handler = None  # break the refcount cycle
        self.parent.throw(LoopExit('This operation would block forever'))
```

这个方法就是协程的入口函数,它内部实际是一个循环, 这个循环就是用来启动libev的 事件循环的. 该函数一般是在调用 `hub.switch`
时开始运行的.

### Waiter

协程间的通信机制.

``` python
class Waiter(object):
    def __init__(self, hub=None):
        if hub is None:
            self.hub = get_hub()
        else:
            self.hub = hub
        self.greenlet = None
        self.value = None
        self._exception = _NONE

    def get(self):
        """If a value/an exception is stored, return/raise it. Otherwise until switch() or throw() is called."""
        if self._exception is not _NONE:
            if self._exception is None:
                return self.value
            else:
                getcurrent().throw(*self._exception)
        else:
            assert self.greenlet is None, 'This Waiter is already used by %r' % (self.greenlet, )
            self.greenlet = getcurrent()
            try:
                return self.hub.switch()
            finally:
                self.greenlet = None

    def switch(self, value=None):
        """Switch to the greenlet if one's available. Otherwise store the value."""
        greenlet = self.greenlet
        if greenlet is None:
            self.value = value
            self._exception = None
        else:
            assert getcurrent() is self.hub, "Can only use Waiter.switch method from the Hub greenlet"
            switch = greenlet.switch
            try:
                switch(value)
            except:
                self.hub.handle_error(switch, *sys.exc_info())

```

该类的实例有一个value属性, 一个\_expception属性, 一个get方法,一个switch方法,他 们的行为是这样的:

1.  get: 当你在一个协程中调用get方法时, 它会先检查\_exception的值,如果不为默 认的\_NONE,
    那么它就会根据value属性的值来决定是返回value的值还是抛出异 常,
    如果\_exception为默认值, 它会设置greenlet属性为当前的协程对象,接着就 会切换到hub协程.
2.  switch: 实际就是调用Waiter对象的greenlet属性的switch方法, 这样就切换到 了对应的协程.
    一般会注册到某个watcher的回调函数. 如果greenlet属性为
    None,那么意味着switch在get之前运行了,那么就简单的设置下value以
    及\_exception属性.

**需要等待的协程调用get方法,这样该协程就会挂起, 其他的协程调用switch方法切换 到因等待而挂起的协程**,
我们来看看Waiter的一个使用例子, Hub的wait方法的代 码:

``` python
class Hub(greenlet):
    ...
    def wait(self, watcher):
        waiter = Waiter()
        unique = object()
        watcher.start(waiter.switch, unique)
        try:
            result = waiter.get()
            assert result is unique, 'Invalid switch into %s: %r (expected %r)' % (getcurrent(), result, unique)
        finally:
            watcher.stop()
    ...
```

**wait方法的作用是挂起当前的协程,直到watcher监听的事件就绪**.它创建一个
Waiter实例waiter,接着将waiter的switch方法注册到wacher上,这样当watcher监听的
事件就绪后就会调用实例的switch方法,接着就调用waiter的get方法, 根据watcher监
听的事件就绪的快慢,这里有两种可能:

1.  **get在switch之前运行**: get会设置waiter的greenlet属性为当前执行的协程, 接着 切换到hub,
    当将来某个时候事件就绪,那么调用waiter的switch,switch会调用
    greenlet属性的switch方法,这样就切换回了当前运行的协程.
2.  **get在switch之后运行**: 这种情况比较少见,可是也是存在的, 这种情况下运行
    switch时,waiter对象的greenlet属性为None, 所以switch方法只是简单的设置
    waiter的value属性, 接着调用get会直接返回value属性,而不阻塞.注意不要弄 混\_NONE与None.

## Greenlet

这也是一个greenlet的子类,它也是用来产生协程的, 我们先来看看我们创建协程时常 用的spawn函数的源码:

``` python
class Greenlet(greenlet):
    def __init__(self, run=None, *args, **kwargs):
        hub = get_hub()
        greenlet.__init__(self, parent=hub)

    @classmethod
    def spawn(cls, *args, **kwargs):
        """Return a new :class:`Greenlet` object, scheduled to start.
        The arguments are passed to :meth:`Greenlet.__init__`.
        """
        g = cls(*args, **kwargs)
        g.start()
        return g
```

gevent.spawn实际就是Greenlet类的spawn方法,该方法直接创建一个Greenlet实例,注
意该实例的parent是hub,而不是默认的主协程, 这样的用处是当协程完成退出时,程序
会继续执行hub的事件循环.然后调用实例的start方法, 下面看看start方法的代码

``` python
def start(self):
    """Schedule the greenlet to run in this loop iteration"""
    if self._start_event is None:
        self._start_event = self.parent.loop.run_callback(self.switch)
```

start方法实际上就是把该实例丢到hub协程的循环当中,也就是说这个新建的协程就可 以被hub调度了.

``` python
def run_callback(self, func, *args):
    CHECK_LOOP2(self)
    cdef callback cb = callback(func, args)
    self._callbacks.append(cb)
    libev.ev_ref(self._ptr)
    return cb
```

上面的代码先创建一个callback实例cb,接着将这个实例放进\_callbacks列表中, 在core
部分我们分析了\_callbacks列表的所有callback实例都会被\_prepare watcher的回调
函数 `gevent_run_callbacks` 运行, 这样实际就是启动了协程.

## socket模块

我们知道为了发挥协程的威力, 我们不能使用标准socket库,必须使用gevent实现的 socket库,
现在我们来分析一下gevent的socket模块,看看该模块是如何使用协程的,
我这里以socket的recv方法为例. 假设调用recv方法的协程为gr.

``` python
class socket(object):

    def __init__(self, family=AF_INET, type=SOCK_STREAM, proto=0, _sock=None):
        if _sock is None:
            self._sock = _realsocket(family, type, proto)
            self.timeout = _socket.getdefaulttimeout()
        else:
            if hasattr(_sock, '_sock'):
                self._sock = _sock._sock
                self.timeout = getattr(_sock, 'timeout', False)
                if self.timeout is False:
                    self.timeout = _socket.getdefaulttimeout()
            else:
                self._sock = _sock
                self.timeout = _socket.getdefaulttimeout()
        self._sock.setblocking(0)
        fileno = self._sock.fileno()
        self.hub = get_hub()
        io = self.hub.loop.io
        self._read_event = io(fileno, 1)
        self._write_event = io(fileno, 2)

```

\_\_init\_\_很简单,创建一个socket(self.\_sock),将该描述符设置为非阻塞,同时创建两个
watcher,分别监听读事件(self.\_read\_event)以及写事件(self.\_write\_event),下面
看看recv的代码:

``` python
def recv(self, *args):
    sock = self._sock  # keeping the reference so that fd is not closed during waiting
    while True:
        try:
            return sock.recv(*args)
        except error:
            ex = sys.exc_info()[1]
            if ex.args[0] != EWOULDBLOCK or self.timeout == 0.0:
                raise
            # QQQ without clearing exc_info test__refcount.test_clean_exit fails
            sys.exc_clear()
        self._wait(self._read_event)
```

recv直接调用内置模块的recv方法,如果发现该调用会阻塞,那么就调用\_wait方法, 该 方法也是代码的关键部分.

``` python
def _wait(self, watcher, timeout_exc=timeout('timed out')):
    """Block the current greenlet until *watcher* has pending events.

    If *timeout* is non-negative, then *timeout_exc* is raised after *timeout* second has passed.
    By default *timeout_exc* is ``socket.timeout('timed out')``.

    If :func:`cancel_wait` is called, raise ``socket.error(EBADF, 'File descriptor was closed in another greenlet')``.
    """
    assert watcher.callback is None, 'This socket is already used by another greenlet: %r' % (watcher.callback, )
    if self.timeout is not None:
        timeout = Timeout.start_new(self.timeout, timeout_exc, ref=False)
    else:
        timeout = None
    try:
        self.hub.wait(watcher)
    finally:
        if timeout is not None:
            timeout.cancel()
```

根据注释我们知道\_wait方法会使当前的协程暂停,直到watcher监听的事件就绪. 代码的 关键部分是
`self.hub.wait(watcher)`, 这个方法在上面已经分析过,只要明白它会阻 塞当前的协程切换到hub协程,
而如果watcher监听的事件就绪,它又会切换会当前协程,
在recv的例子中,一旦wacher监听的事件就绪也就意味着socket已经处于读就绪状态,所
以也就可以调用内置的socket模块的recv方法来获得数据了.

## timeout模块

该模块实现了一个超时机制, 它先挂起当前的协程, 当指定的时间到了之后,它会切换 到该协程,并且在该协程中抛出异常. 这样就实现了挂起协程的目的

``` python
class Timeout(BaseException):
    def __init__(self, seconds=None, exception=None, ref=True, priority=-1):
        self.seconds = seconds
        self.exception = exception
        self.timer = get_hub().loop.timer(seconds or 0.0, ref=ref, priority=priority)

    def start(self):
        """Schedule the timeout."""
        assert not self.pending, '%r is already started; to restart it, cancel it first' % self
        if self.seconds is None:  # "fake" timeout (never expires)
            pass
        elif self.exception is None or self.exception is False or isinstance(self.exception, string_types):
            # timeout that raises self
            self.timer.start(getcurrent().throw, self)
        else:  # regular timeout with user-provided exception
            self.timer.start(getcurrent().throw, self.exception)

    @classmethod
    def start_new(cls, timeout=None, exception=None, ref=True):
        if isinstance(timeout, Timeout):
            if not timeout.pending:
                timeout.start()
            return timeout
        timeout = cls(timeout, exception, ref=ref)
        timeout.start()
        return timeout

    @property
    def pending(self):
        """Return True if the timeout is scheduled to be raised."""
        return self.timer.pending or self.timer.active

    def cancel(self):
        """If the timeout is pending, cancel it. Otherwise, do nothing."""
        self.timer.stop()

```

先看\_\_init\_\_, 它为实例创建了如下属性:

1.  seconds: 超时的秒数, 如果为None,那么永不超时
2.  exception: 超时抛出的异常,如果为None,那么就抛出self本身
3.  timer: 一个timer watcher

在来看start, 它分为三种情况:

1.  self.second为None: 那么直接pass, 这就意味者timer没有注册到时间循环中,所 以也就永远不会超时
2.  self.exception为None: 它会将 `getcurrent().throw` 注册为timer的回调函数,
    我们知道协程对象的throw方法和switch是相似的,都会切换到对应协程,只是throw
    在切换到对应协程后会立刻将它的参数作为异常抛出, 所以一旦超时,那么就会切 换到当前协程,然后抛出self
3.  self.exception不为None, 和2相似,只是超时会抛出self.exception而不是self本 身.

start\_new是一个包装函数, 正常情况下你要先创建一个timeout实例,然后调用该实例 的start方法,
现在你只需要调用这个方法它就会把这两步一起搞定.

### timeout使用指南

一般情况下timeout都是这样使用的

``` python
timeout = Timeout(seconds, exception)
timeout.start()
try:
    ...  # exception will be raised here, after *seconds* passed since start() call
finally:
    timeout.cancel()
```

最开始的两行可以用Timeout.start\_new代替, 在try中间我们一般会切换到其它的协 程,
当超时后会自动切换回来,并且抛出异常,这样try就可以捕捉到了.来看看一个更
具体的例子,event.py中的例子:

``` python
timer = Timeout.start_new(timeout)
try:
    try:
        result = self.hub.switch()
        assert result is self, 'Invalid switch into Event.wait(): %r' % (result, )
    except Timeout as ex:
        if ex is not timer:
            raise
finally:
    timer.cancel()
```

很显然的例子.

## Event

该模块的Event实现了协程间的通知机制, 也就是一个协程可以唤醒监听该event的所 有协程.

### Event使用指南

在一个协程中创建event对象,并调用该对象的wait方法,这样该协程就会阻塞,直到另 外一个协程调用了该event对象的set方法,代码如下:

``` python
# greenlet1
evt = Event()
evt.wait()                # block until other greenlets invoke evt.set()

# greenlet2
evt.set()
```

## AsycResult

## queue

## channel

### channel使用指南

和go语言的channel类似,只是没有缓存也没有类型信息,如果要缓存,那么可能queue 更合适

``` python
# greenlet1
chan = Channel()
val = chan.get()

# greenlet2
chan.put(val)
```

使用方法也是两个协程配合, 一个读一个写,如果channel未就绪,那么相应的读或者 写就会阻塞执行该操作的的这个协程.

## 其它

在Greenlet类的join函数中有如下代码：

``` python numberLines
switch = getcurrent().switch
self.rawlink(switch)

result = self.parent.switch()
```

rawlink的作用是注册一个函数，这个函数会在这个greenlet运行完成后调用

# 第三方库

gevent不像go一样是官方内置的，所以有些时候和第三方库配合会有一些问题，总的来说 python写成的库可以直接monkey
patch，C写成的库可以直接用豆瓣开源的greenify来打 patch。

## greenify

这个库可以将C扩展打patch，这样可以让他们兼容gevent，它直接工作于二进制文件这一级，
不需要你修改C扩展代码，目前只支持ELF文件格式。他会自动的对网络相关的代码来patch。

## PyMongo

有几个注意事项。

1.  只初始化一个 `pymongo.Connection` 对象，最好把这个弄成一个模块级或者全局变量，
    库的内部由pool，所以你不用操心。
2.  至少要monkey patch掉socket和threading模块
3.  要调用 `end_request` 来将连接归还到pool中。
