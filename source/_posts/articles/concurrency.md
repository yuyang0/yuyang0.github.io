---
title: Concurrency
date: 2015-03-15 11:54:17
tags:
- CSP
- Actor
categories:
- articles
---

# CSP

CSP的执行实体是一个一个的process，这些process之间通过channel通信，代表语言是go，
go语言中的goroutine就是执行实体，goroutine之间通过channel通信，注意go语言中
channel是可以有缓冲区.

CSP模型有这几个特点:

1.  process之间通过channel来通信, 注意channel是匿名的，任何process都可以向channel中 读或者写，
2.  channel的读或者写都是同步的，也就是说，如果一个process向channel中写的时候没有
    process在读该channel那么该process就会阻塞，同理读的时候也一样

下面是一个CSP模型的示意图：![](concurrency/static/img/csp_illustration2.png)

# Actor

Actor模型的执行实体是actor，每一个actor都有一个关联的mailbox，每个actor都可以读
取自己的mailbox中的消息，任一actor都可以向其它actor的mailbox发送消息，这样不同的
actor之间就可以通信了。Actor模型的代表语言是erlang。

Actor模型的特点:

1.  actor通过mailbox通信，这种通信是点对点的，也就是说一个actor发送消息时必须指定
    要把消息发给哪一个actor的mailbox，这和CSP的channel的区别很大
2.  actor发送消息时是异步的，也就是说它不会阻塞，当然如果一个actor读它的mailbox,
    而这个时候mailbox中没有消息，那么该actor也是会阻塞的。

下面是一个Actor模型的示意图：![](concurrency/static/img/actor_illustration2.png)
