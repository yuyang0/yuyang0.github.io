---
title: UNP笔记
date: 2011-01-12 11:54:17
tags:
- network
- socket
- api
categories:
- notes
---

# socket地址相关

有一个通用地址结构 `struct sockaddr_in`, bind, connect这样的API中都是使用这个 结构,
其它的地址结构必须类型转换为该结构. ipv6又定义了一个新的通用地址结构 `struct
sock_storage`, 该结构可以包含所有的地址,包括ipv4,ipv6以及unix.

``` c
/* -------------- 通用地址结构(旧) ----------------------- */
struct sockaddr {
    uint8_t     sa_len;
    sa_family_t sa_family;    /* address family: AF_xxx value */
    char        sa_data[14];  /* protocol-specific address */
};
/* ---------------- ipv4 ----------------------------------- */
struct in_addr {
    in_addr_t  s_addr;    /* 32-bit IPv4 address */
    /* network byte ordered */
};
struct sockaddr_in {
    uint8_t         sin_len;       /* length of structure (16) */
    sa_family_t     sin_family;    /* AF_INET */
    in_port_t       sin_port;      /* 16-bit TCP or UDP port number */
    /* network byte ordered */
    struct in_addr  sin_addr;      /* 32-bit IPv4 address */
    /* network byte ordered */
    char            sin_zero[8];   /* unused */
};

/* --------------通用地址结构(新, ipv6定义) ------------------ */
struct sockaddr_storage {
    uint8_t      ss_len;    /* length of this struct (implementation dependent) */
    sa_family_t  ss_family; /* address family: AF_xxx value */
    /* implementation-dependent elements to provide:
     * a) alignment sufficient to fulfill the alignment requirements of
     *    all socket address types that the system supports.
     * b) enough storage to hold any type of socket address that the
     *    system supports.
      */
};

/* ------------------------ ipv6 --------------------------- */
struct in6_addr {
    uint8_t   s6_addr[16];   /* 128-bit IPv6 address */
                             /* network byte ordered */
};
#define SIN6_LEN         /* required for compile-time tests */
struct sockaddr_in6 {
    uint8_t          sin6_len;      /* length of this struct (28) */
    sa_family_t      sin6_family;   /* AF_INET6 */
    in_port_t        sin6_port;     /* transport layer port# */
                                    /* network byte ordered */
    uint32_t         sin6_flowinfo; /* flow information, undefined */
    struct in6_addr  sin6_addr;     /* IPv6 address */
                                    /* network byte ordered */
    uint32_t         sin6_scope_id; /* set of interfaces for a scope */
};
```

1.  有两个函数来转换ip地址,也就是在presentation format与numeric format之间转换,
    presentation format是字符串,比如"127.0.0.1", 这种形式对人更友好,而numeric
    format是数字,它是放在地址结构体中, 这两个函数能转换ipv4以及ipv6地址,原型如 下:
    
    ``` example
    #include <arpa/inet.h>
    
    int inet_pton(int af, const char *src, void *dst);
    const char *inet_ntop(int af, const void *src,
                          char *dst, socklen_t size);
    ```
    
    常用 `inet_pton` 来将字符串表示的ip转换为地址结构体要求的数字ip

2.  因为网络程序需要在不同的机器间通信,所以字节序很重要, 网络协议默认的字节序是 大端字节序, 所以需要函数来在 **主机字节序(host
    byte order)** 与 **网络字节序 (network byte order)** 之间转换, 有以下几个函数:
    
    ``` example
    #include <netinet/in.h>
    uint16_t htons(uint16_t host16bitvalue) ;
    uint32_t htonl(uint32_t host32bitvalue) ;
                           Both return: value in network byte order
    uint16_t ntohs(uint16_t net16bitvalue) ;
    uint32_t ntohl(uint32_t net32bitvalue) ;
                           Both return: value in host byte order
    ```
    
    常用 `htons` 来转换端口号.

## 常用代码

1.  客户端
    
    ``` c
    struct sockaddr_in servaddr;
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(13);
    if (inet_pton(AF_INET, argv[1], &servaddr.sin_addr) < 0) {
        /* error */
    }
    ```

2.  服务端
    
    ``` c
    struct sockaddr_in servaddr;
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(13);
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    /* or use inet_pton to set ip */
    ```

# 基本socket API

## connect

客户端使用该api建立连接,它可能返回以下错误:

1.  ETIMEDOUT: 超时, 也就是说没有接收到server端的ACK报文.
2.  ECONNREFUSED: 发送SYN报文后,收到了RST报文,也就是说server端没有进程在监听 你要连接的端口.
3.  EHOSTUNREACH, ENETUNREACH:收到了ICMP的unreachable的报文

**connect一旦出错,那么该socket就不可用了,必须关闭.**

## bind

## listen

原型:

``` example
int listen (int sockfd, int backlog);
```

内核会维护两个队列:

1.  未完成队列:　当客户端发送３次握手的第一个SYN报文到服务端时,该队列就会增加 一项,完成三次握手后该项就会移到已完成队列的末尾
2.  已完成队列: 当客户端完成三次握手,那么在队列的末尾增加一项,当你调用accept
    时就从队列的开头移除一项,如果该队列为空,那么accept会阻塞.

从linux2.2开始backlog是已完成队列的最大值. 未完成队列的最大值由一个全局值设 定,该值在
/proc/sys/net/ipv4/tcp\_max\_syn\_backlog中, 如果已完成队列满了,而此时
未完成队列的某个连接收到了三步握手的最后一个ACK,
这时内核会忽略这个ACK(意思就 是说就像没有收到这个ACK包一样),
因此一段时间后server端会重发第二步握手的 SYN/ACK包.

## socket读写api

1.  对于面向连接的socket(TCP), 有以下两组api, recv/send以及read/write, 前者比
    后者多了一个flag参数, 当使用flag=0来调用recv,
    send时,他的行为和read,write 是一样的.
2.  面向非连接的socket(UDP, RAW):有以下api, recvfrom/sendto, 这两个有一个地址
    参数和一个指示地址结构长度的参数, 如果将这两个值设为NULL和0,那么行为实际
    和recv/send一样.
3.  send buffer: 在内核中,每一个socket都有一个 send buffer
      - **TCP**: 当你调用write或者send时, 实际就是将数据从用户空间的buffer复制到 内核空间的send
        buffer, 如果send buffer的空间不够,那么write/send操作将阻
        塞(前提是socket没有设为非阻塞).当write/send返回时,意味着数据已经都复制
        到了send buffer,但不保证数据已经通过网络发送出去了. 只有收到对方的ACK报 文,这些数据才能从send
        buffer移除.
      - **UDP**: 因为udp是直接将数据加上udp头部发往下层,也就是udp数据报要原子的发
        送,它不能拆解,所以如果你传入的数据太大,比如比send
        buffer还大,那么内核会 返回错误EMSGSIZE. 如果只是当前的send buffer没有足够的空间,那么阻塞的
        socket上的send调用会阻塞,非阻塞的send调用会返回EAGAIN或者EWOULDBLOCK.
        **UDP上的send要么全部发送,要么出错,不会出现只发送一部分数据的情况**
4.  receive buffer: 内核中每一个socket都有一个receive buffer.
      - **TCP**: 通过tcp的流控制(tcp报文的Window字段), 可以通知peer该端目前能够接
        受的数据大小,如果peer忽略该建议,同时发送来的数据大于该大小,那么内核会丢
        弃这个数据报.因为receive buffer没有足够的空间容纳这些数据.丢弃后peer过
        一段时间要重发,那么peer就会"慢下来", 这实际就是流量控制.
      - **UDP**: udp没有流控制,如果receive buffer,内核会直接丢弃数据报
5.  读就绪条件:满足下面四个条件之一的,read操作不会阻塞
    1.  socket的receive buffer中的数据大于等于"低水位"值(low-water mark)
    2.  socket的被读关闭,那么read会马上返回0
    3.  socket是listening socket,而且这时已完成队列不为空,那么accept会立即返回
    4.  socket上有错误在排队,read操作会直接出错返回
    **当读就绪时, 阻塞读与非阻塞读的行为是相似的,都会立即返回.并且返回读到的字 节数.**
6.  写就绪条件: 满足下面四个条件之一的,write操作不会阻塞
    1.  socket的send buffer的空余空间大于"低水位"值,
    2.  socket被写关闭,那么写操作会产生SIGPIPE信号.
    3.  非阻塞的connect,或者connect出错
    4.  socket上有错误在排队, write会直接出错返回.

# socket options

使用下面的两个系统调用来获取以及设置socket选项

``` example
#include <sys/socket.h>
int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
int setsockopt(int sockfd, int level, int optname, const void *optval socklen_t optlen);
```

注意optval对于不同的选项是不同的值,所以你要optlen参数来通知获得optval的长度.

## SO\_KEEPALIVE

如果socket设置了该选项, 那么当一个连接有两个小时没有交换数据了(这意味着程序 阻塞在某个系统调用上),
那么就会向对方发送一个keep-alive probe, 实际就是一个 tcp报文.
如果这个报文收到了正常的ACK,那么就什么也不做,如果超时或者收到RST报
文,那么被阻塞的系统调用会返回相应的错误.

## SO\_LINGER

对面向连接的协议有效, 所以对UDP该选项无用, 该选项是用来改变socket调用close时的行
为.默认如果你调用close,那么close会立即返回,但是如果send
buffer有数据,那么系统会 现将数据发送出去. 这种默认行为可以改变

## SO\_RCVBUF/SO\_SNDBUF

修改socket的send buffer以及receive buffer的大小. **这两个选项必须在connect与
listen之前设置**.

## SO\_RCVLOWAT/SO\_SNDLOWAT

低水位标志,用来判断一个socket是否可读或者可写, 如果接受缓冲区的数据量大于 SO\_RCVLOWAT(默认为1)那么可读,
如果发送缓冲区的空闲空间大于SO\_SNDLOWAT(默认通常为 2048)那么可写.

## SO\_REUSEADDR

### tcp

1.  可以避免重启服务器时的 address already in use 的错误
2.  指定该选项,那么可以在相同的端口上启动多个实例,也就是多个socket bind到相同 的端口,但是ip地址必须不同
3.  即便指定该选项,tcp也不能bind完全相同的地址(相同的ip与port)到多个socket,如 果要这么做,那么需要使用
    `SO_REUSEPORT` 选项.

### udp

如果指定了该选项,那么你可以bind完全相同的地址(相同的ip与port)到多个socket, 当
收到数据报时,如果你指定的是多播地址,那么数据报会发送给每一个绑定的socket,如果
是单播地址,那么随机的选择一个socket.对单播地址的场景, **建议使用SO\_REUSEPORT**,
因为它可以将数据报更均衡的发布到多个socket上.

## SO\_REUSEPORT

这个选项用来将完全相同的地址bind到多个socket,支持tcp,udp

### tcp

通常的tcp服务器都是这样的:

1.  一个线程创建listen socket,然后accept,得到连接后下发到工作线程,memcached就
    是这么实现的,但是它有一个问题就是这个accept的线程在极端场景可能成为瓶颈.
2.  多个线程同时在一个listen socket上accept,哪个线程得到新连接那么它就负责处
    理.这种方式的问题是负载不均衡.也就是说有的线程得到的连接多,有的得到的少.

上面的两种做法的问题根源是 **只能有一个listen socklet**, 通过指定 `SO_REUSEPORT`,
可以创建多个绑定到相同地址的listen socket, 这样你可以在不同
的进程或者线程里面单独的accept到该地址的新连接,操作系统负责将新连接均匀的分 布到各个listen
socket.

### udp

单个socket的recv buffer是有锁的,所以多线程的使用单个socket是得不到明显的性能 提升的.甚至可能使性能下降,通过指定
`SO_REUSEPORT` 你可以创建多个绑定到相同地 址的udp
socket,这样你就可以在不同的线程或者进程中单独的收发到该地址的数据报.因
为使用的是不同的socket,所以也就不会用锁竞争的问题.

# 编程技巧

## 一些情形下tcp socket的行为

  - server端的进程终止了,这时client端向socket写则会收到RST报文, 如果你接着向这个
    socket写入数据,那么会收到SIGPIPE信号.如果你忽略了该信号,那么write会返回EPIPE
    错误.
  - 如果server崩溃(比如断电,比如断掉网线然后关掉进程, 总之让client无法收到正常 关闭时的FIN包), 这时有两种情况:
      - 如果server端没有恢复正常(比如没有重启, 或者没有插上网线重启进程), 那么 client端会超时.
      - 如果server恢复了正常,那么client向server写会收到RST, 这时读该socket会产生 ECONNRESET错误
  - RST的产生条件：
    1.  connect连接一个服务器，可是服务器没有运行（在指定的端口没有 socket在listen），
        该情况作为connect调用的错误检查（ECONNREFUSED）
    2.  当tcp要 abort一个连接时。
    3.  当tcp收到一个不存在的连接发来的数据包时。
  - 对于连接的任一端，如果发送FIN，就意味着该端不会再向该连接写数据，所以另一端 read该连接都会返回0也就是EOF。
  - Connect只能调用一次，不能像accept，read，write那样被信号打断后重启，一旦失败， 那么该socket
    fd就必须关闭，当然只是同步调用，如果指定了了O\_NOBLOCK,如果connect
    失败，且errno=EINPROGRESS，那么并不意味在调用失败。
  - 如果向socket写入数据返回了RST，那么此时读该socket会返回ECONNRESET错误，如果此
    时写该socket会产生SIGPIPE信号，该信号默认会终止该程序，所以必须处理。
  - 如果一个A到B的连接，如果执行了B-\>A的半关闭，也就是B向A发送FIN，那么表明B不会再
    向socket写数据，所以在A端读该socket会返回EOF，如果A端向socket写，那么分情况：
    1.  如果B端的进程已终止，那么连接已经不存在，所以会返回一个RST，
    2.  如果B端的进程没有终止，而只是执行了半关闭，那么A端的write是合法的，因为 半 关闭是合法的。
  - 对于tcp客户端，如果没有明确调用bind，那么socket的端口是在调用connect时由内核随 机指定，而udp
    socket的端口则是在第一次调用sendto 时由内核随机指定的，而且一旦 指定就不在改变，而udp
    socket的ip地址是可变的，如果客户端主机是一个多接口（有多
    块网卡）的主机，那么每一次发送udp数据包内核都可以随机选择可用的ip地址。（ip地
    址是在ip层指定，而端口则是在udp层指定，所以没有调用bind的socket，它的端口不可 变，ip却可变）。
  - 对于缓冲区：对tcp而言，向socket写，会将数据从用户缓冲区复制到内核的发送缓冲区，
    然后内核将这些数据发送给目的端，但是此时内核发送缓冲区的数据没有丢弃，只有当接
    到ack时这些数据才会丢弃，因为网络数据包可能丢失，所以这些可能需要重传。而对于
    udp则并没有发送缓冲区，因为udp是unreliable的，它无连接，所以不需要重传，直接将
    数据加上udp头，ip头发送出去，然后数据就丢弃了。对于内核的接收缓冲区，tcp与udp
    基本一致，udp的内核接受缓冲区会限制接受的数据报的数量，一旦
    超出缓冲区的大小， 后续的udp包都会丢弃。（unp 8.13）。

## 异步非阻塞

1.  `connect`: 在调用之前将socket设置为非阻塞, 那么connect可能会出错, 那么这时候 要检查errno,
    如果为EINPROGRESS, 那么意味着connect需要阻塞, 这时候你应该监听 socket的可写事件,
    如果发现可写,那么使用 `getsockopt` 来获取错误, 如果没有错误,那 么connect执行成功,
    否则执行失败,示例代码
    
    ``` c
    int flags;
    
    if((flags = fcntl(fd, F_GETFL)) < 0) //获取当前的flags标志
      err_sys(“F_GETFL error!”);
    
    flags |= O_NONBLOCK; //修改非阻塞标志位
    
    if(fcntl(fd, F_SETFL, flags) < 0)
      err_sys(“F_SETFL error!”);
    if (connect(fd, (struct sockaddr*)&sa, sizeof(sa)) == -1) {
      if (errno != EINPROGRESS) {
        return -1;
      }
     }
    // 监听可写事件, 如果可写执行下面代码
    err = 0;
    errlen = sizeof(err);
    if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &errlen) == -1) {
      sprintf("getsockopt(SO_ERROR): %s", strerror(errno));
      close(fd);
      return ERR;
     }
    if (err) {
    
      errno = err;
    
      close(fd);
    
      return ERR;
     }
    ```

2.  服务端在accept之前将socket设置为非阻塞, 这样你就可以监听listenfd的可读事件,
    如果可读,那么就意味着有新连接进来,那么你就可以调用accept来获得连接了,
    如果没 有连接的时候你调用accept, 那么会出错返回, errno为EAGAIN或者EWOULDBLOCK.

## raw socket

raw socket 是用来获取IP报文的, 但是内核不会把所有的报文都发给raw socket.

1.  TCP/UDP的IP报文不会发给raw socket, 所以要抓这种报文只能去链路层抓包
2.  ICMP/IGMP的IP报文会发送给raw socket
3.  如果内核不认识IP报文的协议类型字段,那么发给raw socket
4.  如果IP报文有分片,那么只有把所有的分片组合成一个完整的IP报文后才会发送给raw socket.
