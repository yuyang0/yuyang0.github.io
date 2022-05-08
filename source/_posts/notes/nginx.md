---
title: Nginx
date: 2015-02-12 11:54:17
tags:
- nginx
categories:
- notes
---

# nginx的安装

## 安装

可以直接使用ubuntu仓库中的nginx,不过版本有点陈旧,下面说一说编译安装, 编译安 装是非常典型的configure, make,
make install模式, 我只说一说configure的常用选 项.

1.  –prefix: nginx的安装目录
2.  –with-XXX\_XXX: 需要开启的模块

添加第三方模块,使用如下命令:

``` example
./configure --add-module=/path/to/your-module
```

## 重新编译安装

有时候我们发现nginx缺少模块,需要重新编译安装, 那么使用如下步骤:

1.  使用configure, make编译, 不要运行make install, 因为这是重新安装会覆盖掉我 们的配置文件
2.  将得到的nginx二进制程序直接覆盖旧版本的nginx程序,该程序一般在sbin目录,为 了保险,你可以先备旧版的nginx程序.
3.  使用 `nginx -t` 测试nginx程序是否正确
4.  使用 `nginx -s reload` 来重载nginx.

## nginx的配置目录布局

使用ubuntu的apt安装的nginx的配置一般有如下的目录格式

1.  nginx.conf包含了基本的http配置，同时会有如下代码：
    
    ``` example
    http {
    
        .....
    
        include /etc/nginx/conf.d/*.conf;
        include /etc/nginx/sites-enabled/*;
    }
    ```
    
    可以看到它会包含conf.d 目录的配置，以及sites\_enabled目录的所有配置

2.  conf.d： 该目录的所有conf文件都会被包含

3.  sites-available: 这个目录的每一个文件应该配置一个server, 也就是一个虚拟机,
    其中有一个默认的配置default.

4.  sites-enabled: 都是符号链接，指向sites-availabe目录的配置，通过这些符号链
    接你可以有选择的开启sites-availabe中的server。

# nginx的基础知识

## 基本术语

1.  upstream（上游）: 是指nginx后端的服务, 比如tornado, redis等等
2.  downstream(下游): 指浏览器, 或者其它向nginx请求服务的程序.
3.  主请求(main request): 由 HTTP 客户端从 Nginx 外部发起的请求.
4.  子请求(subrequest): 由 Nginx 正在处理的请求在 Nginx 内部发起的一种级联请 求, 子请求的变量是独立的,
    和父请求互不干扰.
5.  内部跳转: 使用rewrite,echo\_exec等指令执行的跳转, 它的作用原理是直接修改 url, 然后跳回find config
    phase, 所以它就会使用修改后的url去匹配location, 然后接着执行rewrite等一系列phase.

## phase

一个http请求会依次经过如下11个phase:

1.  post read: 接收完请求头之后的第一个阶段,它位于uri重写之前, 实际上很少有 模块会注册在该阶段,默认的情况下,
    该阶段被跳过.
2.  server rewrite: server级别的uri重写阶段, 如果set, rewrite等ngx\_rewrite模
    块的指令放在server内,但在location外,那么就运行于该阶段.
3.  find config: 寻找location配置阶段,该阶段使用重写之后的uri来查找对应的
    location,值得注意的是该阶段可能会被执行多次,因为也可能有location级别的重
    写指令, **该阶段不允许模块注册**.
4.  rewrite: location级别的uri重写阶段,该阶段执行location基本的重写指令,也 可能会被执行多次. set,
    rewrite等指令如果放在location内部,那么就运行于该阶 段.
5.  post rewrite: location级别重写的后一阶段, 用来检查上阶段是否有使用rewrite 指令来i进行uri重写,
    如果有那么就跳回find configphase, 这就是一个内部跳转, **该阶段不允许模块注册**.
6.  preaccess: 访问权限控制的前一阶段，该阶段在权限控制阶段之前，一般也用于访 问控制，比如限制访问频率，链接数等；
7.  access: 访问权限控制阶段, 比如基于ip黑白名单的权限控制, 基于用户名密码的 权限控制等.
8.  post access: 访问权限控制的后一阶段,该阶段根据权限控制阶段的执行结果进行 相应处理. **该阶段不允许模块注册**.
9.  try files: try\_files指令的处理阶段，如果没有配置try\_files 指令，则该阶段被跳过,
    **该阶段不允许模块注册**.
10. content: 内容生成阶段, 该阶段产生响应, 并发送到客户端. 如果location中没有
    任何像echo,content\_by\_lua这样运行于content phase的指令,那么nginx会把当前 请求的
    URI 映射到文件系统的静态资源服务模块, 如果文件系统没有对应的文件那 么返回404
11. log: 日志记录阶段, 该阶段记录访问日志.

还有一个特殊的phase: **body filter**, 严格的说它不是一个phase, 任何时候输出响应 体时都会运行该阶段的指令.
该phase可能执行多次,因为输出时是分多次写的,一次一 个chunk.

## phases与指令的关系

几乎所有的指令都与某一个phase相关联, 指令的实际执行顺序是这样决定的:

1.  如果两条指令在不同的phase, 那么指令的执行顺序与phase的先后顺序相同, 比如 set运行于rewrite phase,
    而echo运行于content phase, 所以不论配置中set, echo的顺序如何, set必定在echo的前面执行.
    **建议书写配置时按照指令实际执行 顺序来书写**.
2.  如果两条指令运行于相同的phase, 那么它们的执行顺序就比较混乱了. 有几种情 况:
    1.  有些指令是按照书写顺序来执行的, 比如set, set\_unescape\_uri, set\_by\_lua 等
    2.  绝大多数第三方模块只能运行于所属phase的结尾,所以内置模块的指令必定早于 第三方模块指令的执行,
        因此rewrite必定在rewrite\_by\_lua前面执行.
        set\_by\_lua之所以能和内置的set指令交叉执行,是因为使用了一些特殊技巧
    3.  content phase只能注册一个处理器,所以不要同时使用不同模块的content指令,
        比如echo指令就不能与content\_by\_lua指令共存,而且同一模块的content指令能
        不能多次使用也是依模块而定, echo指令可以多次使用, content\_by\_lua就不行。

## nginx如何匹配请求

实际先匹配server块, 接着匹配该server块中的location.

### 匹配server块

1.  匹配listen指令
      - 每个server块的listen指令如果不完整, 比如缺少ip或者缺少port, 那么用默认 值补充完整,
        ip的默认值是0:0:0:0, port的默认值是80.
      - 使用请求的ip与port来匹配server块的listen指令, 如果最后只有一个server匹
        配那么就使用该server处理请求,
        如果有多个server那么就检查server\_name指令.
2.  匹配server\_name指令（前提是有多个server的listen指令成功匹配请求的ip与 port）
      - 获取请求的Host请求头
      - 先精确匹配, 如果匹配成功那么选择该server, 如果有多个那么选第一个
      - 匹配有\*通配符 **前缀** 的server\_name, 比如 `*.example.com`, 通配符必须 在前面
      - 匹配有\*通配符 **后缀** 的server\_name, 比如 `www.example.*`, 通配符必须 在后面
      - 正则匹配
      - 使用默认的server

### 匹配location

实际上是根据location后面的url来匹配, 该rul会有一些前缀来标示url的类型(正则 或者非正则,
非正则会区分是精确匹配或者前缀匹配, 正则规则又会区分大小写),
具体的规则如下:

1.  无前缀: 匹配前缀(非正则)
2.  ^\~ : 匹配前缀, 如果有一个 正则规则也匹配这个uri,优先采用该规则(非正则)
3.  \= : 精确匹配, 也就是说要一模一样(优先级最高, 非正则)
4.  \~ : 正则,区分大小写
5.  \~\* : 正则, 不区分大小写
6.  \!\~ : 和 \~ 作用相反
7.  \!\~\* : 和 \~\* 作用相反

匹配规则的优先级:

1.  先进行非正则匹配
      - 如果有 =前缀 规则匹配, 那么使用该规则, 如果没有那么看下一条
      - 如果最长前缀匹配的规则含有 ^\~ 前缀, 那么使用该规则, 如果没有看下一条
      - 将最长前缀匹配的规则保存起来, 然后使用正则表达式来匹配
2.  正则匹配, 直接根据位置的从上到下来匹配,第一条匹配的规则会被使用, 如果没有
    匹配,那么就使用上面保存的最长匹配的非正则规则,如果该规则也不存在那么404.

大体上你可以认为优先级是: =规则 \> ^\~规则 \> 正则规则 \> 无前缀规则

``` bash
location = / {}                 # 只匹配/, 不会匹配/xxx
location / {}
location ^~ /images/ {}
location ~* \.(gif|jpg|jpeg)$ {} # 匹配所有图像文件
```

# nginx配置

nginx配置的基本结构是:

``` example
......
events {
......
}

http {
......
   server {
   ......
      location ... {
      ......
      }
      location ... {
      ......
      }
    }

   server {
   ......
   }
......
}
```

## 最外层

1.  user: 指定worker进程的用户名以及用户组
2.  worker\_processes: woker进程的数量,一般和CPU的核数一样
3.  worker\_rlimit\_nofile: worker进程能打开的文件描述符数量.

## events

主要是配置事件相关

1.  worker\_connections: 允许的连接数
2.  use epoll : 事件循环,linux一般是epoll.

## http

一些http相关的配置

## server

配置虚拟机, 最重要的是它的listen,server\_name指令, 很多主机商就是用这种配置来
用一台机器服务多个网站的,配置多个server块,
每一个块的server\_name改为对应的域 名, 那么就可以支持多个网站.

## location

这个块主要是指定对特定url的操作, location后面有一个url参数, 该参数就是用来匹
配http请求的url的,如果匹配成功,那么就执行对应的块中的指令.

# nginx的变量

nginx变量是在nginx启动时创建, 而在请求中赋值的,变量在每一次请求中都有一个单独 的副本(**子请求也会有单独的副本**),
同时变量的生命周期与请求绑定,与location块无 关,不要想当然的认为它和程序语言的静态作用域相同.请看如下例子:

``` bash
server {
    listen 8080;
    location /foo {
         set $a hello;
         echo_exec /bar;
    }
    location /bar {
         echo "a = [$a]";
     }
 }
```

在nginx启动时,它发现了set指令,所以它会创建$a这个变量, 可是这时变量是空的,同时 该变量全局可见,每一个请求都会有一个该变量的副本,
所以你访问/bar会得到空值, 而 访问/foo会得到hello, 因为你在/foo中对$a进行了赋值.

## nginx的内置变量

1.  $request\_uri: 未解码的uri,包含query string
2.  $uri: 已解码的uri, 不包含query string
3.  $arg\_XXX: 用来获得get传递的参数, 将XXX替换未参数名. 比如访问
    `http://localhost:8080/test?name=Tom&class=3`, 那么$arg\_name的值就是
    Tom.注意返回的值是未解码形式.
4.  $args:　请求中问号后面的部分, 也就是query string

## 常见的指令

1.  set: 创建nginx变量并且赋值(rewrite phase)
2.  set\_unescape\_uri: 创建nginx变量,但是会将值解码后赋给该变量(rewrite phase)
3.  set\_escape\_uri: 创建nginx变量,但是会将值编码后赋给该变量(rewrite phase)

# nginx常用模块

## map(ngx\_http\_map\_module)

1.  map: `map string $variable { ... }`
    
    ``` bash
    map $http_user_agent $bad_client {
        default            0;
        "~*Baiduspider"    1;
        "~*360Spider"      1;
        "~*Yandex"         1;
    }
    ```
    
    如果请求user\_agent匹配baidu, 360, yandex特征,那么将bad\_client变量设为1,默 认为值0,
    这样你可以在配置中使用 `if ($bad_client) { return 403; }` 来拦截 这些机器人的访问.

## GEO(ngx\_http\_geo\_module)

可以根据客户端的ip来设置变量的值, 只能放在http块中

``` bash
geo $geo {
    default        0;

    127.0.0.1      2;
    192.168.1.0/24 1;
    10.1.0.0/16    1;

    ::1            2;
    2001:0db8::/32 1;
}
```

根据客户端的ip会对geo变量赋值. 这在拦截一些ip地址是有用. 和map配合可以拦截ip 与user-agent,
可以看这个[nginx-blacklist](https://github.com/oohnoitz/nginx-blacklist)
项目.

## Access(ngx\_http\_access\_module)

用来对请求进行权限检查

1.  allow
2.  deny

会书写顺序检查allow,deny指令，直到第一个匹配。如果有不同的模块也加入了检查比 如access\_by\_lua, 那么
`satisfy` 指令可以规定这些access模块如何协作， 该指令 的值只能为all或者any。

  - all： 必须通过所有access模块的检查（默认）
  - any： 只要通过任何一个access模块的检查就好

## Rewrite(ngx\_http\_rewrite\_module)

该模块的指令如果放在server中,那么运行在server rewrite阶段, 如果放在location 中,那么就运行在rewrite阶段

1.  set: 创建变量

2.  if: `if (condition) { ... }`, if是非常诡异的指令,能不用就尽量不用,一般来说
    if只有在location中,并且在以下两种情况才100%正确:
    
    ``` bash
    if(condition){
        return status_code
    }
    
    if(condition) {
        # other things
        rewrite xxx xxx             # must stay last in if block
    }
    ```

3.  return: 原型如下
    
    ``` example
    return code [text];
    return code URL;
    return URL;
    ```
    
    直接终止请求, 返回相应的code,或者重定向到url(30X)

4.  rewrite: 用来重写url

5.  break: 如果在if中,那么跳出if, 如果在location,那么忽略后面的Rewrite模块的 指令,直接进入下一phase.

## Echo(ngx\_http\_echo\_module)

这是第三方模块,提供了一些类似于shell的命令, 该模块的大部分指令运行于content phase. 常用的指令:

1.  echo: 输出内容到客户端(content phase)

2.  echo\_exec: 执行内部跳转(类似于rewrite,不过运行于content phase)

3.  echo\_location: 执行一个子请求(HTTP GET), 将子请求的结果发送到client,有点类
    似于ngx\_lua模块中的指令ngx.location.capture.
    只是ngx.location.capture更灵 活,因为它可以做后续处理,而不是直接将结果发送到client.

4.  echo\_location\_async: 和echo\_location不过是异步的(HTTP GET), 也就是说主请
    求很有可能在子请求之前结束. 但是nginx会保证输出指令的执行顺序:
    
    ``` bash
    location /main {
            echo_reset_timer;
            echo_location_async /sub1;
            echo_location_async /sub2;
            echo "took $echo_timer_elapsed sec for total.";
        }
        location /sub1 {
            echo_sleep 2; # sleeps 2 sec
            echo hello;
        }
        location /sub2 {
            echo_sleep 1; # sleeps 1 sec
            echo world;
        }
    ```
    
    main虽然可能在子请求的前面结束,但是sub1, sub2,main的输出顺序nginx会保证, 因此输出会是:
    
    ``` example
    hello
    world
    took 0.000 sec for total.
    ```
    
    该指令有点类似于ngx\_lua模块的ngx.location.capture\_multi指令.

# nginx与lua

将lua解释器嵌入到nginx, 使用lua可以比较轻松的实现一些业务逻辑, 这样就可以将
nginx从一个单纯的http服务器变成一个web应用服务器,在配合nginx高效的IO模型,性能
十分强大. 对于逻辑不复杂的业务直接使用lua在nginx这层实现是个不错的选择.

## 安装

依赖于luajit,也可以使用lua, 不过luajit性能更好, ubuntu上可以使用如下命令安装

``` bash
sudo apt-get install libluajit-5.1-dev
```

同时也依赖 `ngx_devel_kit` 这个模块, 这个模块即便是tengine也需要静态编译,因 为它不是http模块, 这个模块会被
`set_by_lua` 指令用到.

1.  nginx安装
    
    ``` bash
    ./configure  --add-module=/path/to/ngx_lua \
                 --add-module=/path/to/ngx_devel_kit
    ```

2.  tengine上安装
    
    ``` bash
    ./configure  --with-http_lua_module \
                 --add-module=/path/to/ngx_devel_kit
    ```

## 常用指令(nginx配置中使用)

1.  set\_by\_lua: 作用于rewrite phase, 和set指令类似, 只是值是由lua脚本产生的,
    该指令可以和set交叉运行
2.  rewrite\_by\_lua: 作用于nginx的rewrite phase. 运行于rewrite phase的最后,
    ngx\_rewrite模块的指令总是先于该指令.
3.  access\_by\_lua: 作用于nginx的access phase, 一般来说如果需要在请求开始之前
    做一些处理,比如检查请求合法性,可以使用这条命令
4.  content\_by\_lua: 作用于content phase, 是用来生成发送给客户端的内容的
5.  log\_by\_lua: 作用于log phase, 一般用来对请求做一些善后处理,记住在该指令之
    前nginx已经将内容发送到客户端.

## lua中能使用的API

ngx\_lua暴露给lua的api都被放在ngx这个全局变量中,这是官方[文档](http://wiki.nginx.org/HttpLuaModule)
其中比较常用的有 这些,(**特别要注意这些api所在phase,这是使用任何nginx模块都需要特别注意的**)

1.  ngx.var.XXX: 所有nginx的变量都可以通过这种方式访问

2.  ngx.ctx: 这是一个表, 它的生命周期和请求绑定, 请求结束了,其中记录的数据也
    就无效了,经常用来在运行于不同的phase的lua脚本间传递数据.

3.  ngx.location.capture: 发起一个子请求, 一些场景很有用,比如说你对
    redis,mysql等服务进行了rest包装,而在一个请求中你想从多个服务中获得数据,就
    可以用这个api发送子请求
    
    ``` example
    location = /memc {
        internal;
        memc_pass ...;
    }
    location = /api {
        content_by_lua '
            local resp = ngx.location.capture("/memc")
            if resp.status ~= 200 then
                ngx.exit(500)
            end
            ngx.say(resp.body)
        ';
    }
    ```
    
    在 `/api` 中你可以获得 `/mem` 的结果, 当然你可以接着获得其它服务的结果.

4.  ngx.location.capture\_multi: 和capture类似,只是可以并发的发起多个子请求
    
    ``` example
    location = /api {
        content_by_lua '
            local res1, res2, res3 =
                ngx.location.capture_multi{
                    {"/memc"}, {"/mysql"}, {"/postgres"}
                }
            ngx.say(res1.body, res2.body, res3.body)
        ';
    }
    ```
    
    并发的发起三个子请求.

5.  ngx.status: 当前请求的response的HTTP状态码, 你可以修改状态码,但是前提是你
    必须在nginx将response发送给client之前修改

6.  ngx.req.XXX: 用来操作request的一系列api

7.  ngx.say: 每一条命令都有一个换行符

8.  ngx.print: 和say类似,没有换行符.

9.  ngx.log: 记录日志

10. ngx.exec: 发起一个内部跳转(浏览器的url不改变), 类似于nginx的rewrite指令

11. ngx.redirect: 发起一个浏览器跳转(30X, 浏览器的url会改变)

12. ngx.exit(status\_code): 直接退出请求

13. ngx.re.XXX: 正则相关,标准lua本身是不支持正则的.

## lua数据共享

1.  请求级别: 使用ngx.ctx, 它是一个table, 你可以使用它来在同一请求的不同的 phase间传递数据
2.  worker进程级别: 可以创建一个lua模块,在模块内部创建一个表,因为lua解释器在 worker进程启动后就常驻内存,
    所以这个表在进程内部一直有效. 因此你可以使用 这种方法在该进程处理的不同请求间共享数据.
3.  nginx级别: 因为nginx是多进程架构, 为了在多个worker进程间共享数据必须用到 IPC机制,
    ngx\_lua使用share memory实现了一个ngx.shared.DICT, 内部是使用自旋
    锁来避免竞争条件,该结构有get,set,incr,delete等api,
    使用比较简单, 为了高效 的利用此api, 数据的大小应该尽量一致,特别不同的数据应该用不同的dict存放,主要
    是为了减少碎片

## lua socket

ngx\_lua实现了一个socket库,它能以非阻塞的形式进行网络操作. 利用这个库我们可以 直接访问upstream的服务,比如redis,
memcache等, 也可以访问tornado这样的上游服 务,只不过tornado需要使用http协议而已,
**注意该库的api基本都只能运行于 rewrite\_by\_lua,
access\_by\_lua, content\_by\_lua,ngx.timer中**, 该库的api分为两 类tcp和udp.

### TCP SOCKET

1.  创建tcp socket对象, 使用ngx.socket.tcp来创建, `local sock = ngx.socket.tcp()`

2.  connect: 可以使用hostname/ip, port, 也可以使用unix socket文件
    
    ``` lua
    local ok, err = sock:connect("www.google.com", 80)
    local ok, err = sock:connect("220.181.57.216", 80)
    local ok, err = sock:connect("unix:/tmp/memcached.sock")
    if not ok then
       check_error()
    end
    ```
    
    如果使用域名,那么必须在nginx.conf中配置dns, 加入这一行: `resolver
            8.8.8.8;`, dns常见的有这几个:
    
      - google: 8.8.8.8
      - alibaba: 223.5.5.5 或者 223.6.6.6
      - 114DNS: 114.114.114.114

3.  send: 发送数据, 一定要先设置超时, 如果成功那么返回发送的数据量,如果失败 那么返回nil,以及一个错误消息字符串,
    出错时socket会自动关闭,无需手动清理
    
    ``` lua
    sock:settimeout(1000)  -- one second timeout
    local bytes, err = sock:send(request)
    ```

4.  receive

很常规的client编程模式,上述调用都是非阻塞的,它对使用者来说是透明的, 下面是 示例代码:

``` lua
sock:settimeout(1000)  -- one second timeout
local line, err, partial = sock:receive()
if not line then
   ngx.say("failed to read a line: ", err)
   return
end
ngx.say("successfully read a line: ", line)
```

### UDP SOCKET
