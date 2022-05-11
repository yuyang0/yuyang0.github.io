---
title: Database
date: 2022-05-08 11:54:17
tags:
- Database
- raft
- paxos
- leveldb
categories:
- note
index_img: https://i.ytimg.com/vi/U5aeM5dvUpA/maxresdefault.jpg
---
# 事务

事务有四个特性(ACID):

## 一致性

也就是说不能破坏数据库的某种约定，比如经典的银行转账问题中，从账户A转100元到账户
B，那么一致性要求两个账户的金额的和要是相同的，数据库不应该让用户看到事务执行的
中间状态, 也就是说不能让用户看到A账户减了100元，但是B账户还没有增加100元这样的中
间状态，一致性有些需要数据库保证，有些则需要应用保证。

## 原子性

只是用来保证事务中的操作完全成功或者全部失败，不可能一部分成功一部分失败

## 持久性

一个事务如果已经提交成功，那么不管数据库出现什么问题，事务所做的修改都不应该丢失

## 隔离性

实际上ACD就已经可以保证一个事务的正常执行，但是ACD可能使数据库非常的低效，所以就
引入了隔离级别，隔离性和一致性是一对矛盾的指标，在最低的隔离等级下，用户是有可能
读到脏数据的，也就是说读到事务的中间状态，这实际就是说在最低的隔离等级下，数据库 是不满足一致性的。数据库的隔离级别如下：

| 隔离级别             | 脏读  | 不可重复读 | 幻读  |
| ---------------- | --- | ----- | --- |
| Read uncommitted | Yes | Yes   | Yes |
| Read committed   | No  | Yes   | Yes |
| Repeatable read  | No  | No    | Yes |
| Snapshot         | No  | No    | No  |
| Serializable     | No  | No    | No  |

  - 未提交读(Read Uncommitted)：允许脏读，也就是可能读取到其他会话中未提交事务修改的数据
  - 提交读(Read Committed)：只能读取到已经提交的数据。Oracle等多数数据库默认都是该级别 (不重复读)
  - 可重复读(Repeated
    Read)：可重复读。在同一个事务内的查询都是事务开始时刻一致的，InnoDB默认级别。在SQL标准中，该隔离级别消除了不可重复读，但是还存在幻象读
  - 快照隔离(Snapshot):
    在事务开始时获得一个快照，事务中的所有read都是读该快照，通常通过MVCC实现，所以它没有脏读，不可重复读以及幻读的问题，它能够处理写写冲突，它主要的问题是写偏斜（write
    skew），
    也就是说两个事务读取相同的数据项，但是更新不同的数据项，比如某人有两个账户，每个账户100元，银行要求两个账户的余额之和大于等于0，那么就会有这种情况出现：
    T1，T2两个事务同时读取两个账户余额，接着都取出200元，T1更新第一个账户，T2更新第二个账户，这样就会出问题。
  - 串行读(Serializable)：完全串行化的读，每次读都需要获得表级共享锁，读写相互都会阻塞

### 快照隔离

快照隔离基本都是通过MVCC实现。

1.  正确性保证：
    
    1.  Consistent Snapshot：所谓Consistent，即快照包含且仅包含Commit先于SnapshotTS的所有事务
    2.  Commit Total Order：所有事务提交构成一个全序关系，每次提交都会生成一个快照，由`CommitTS`标识
    3.  Write-Write Conflict: 事务Ti和Tj有冲突，即它们WriteSet有交集，且`[SnapshotTS, CommitTS]`有交集

2.  单机实现
    
    一个简单的KV单机存储上实现快照隔离的方法：
    
    1.  事务开始时生成一个唯一的事务号，数据的版本号就是事务号
    2.  数据Key都带上版本号，一般直接把版本号放到key的末尾，encode的时候要保证顺序一致
    3.  事务开始时要记录当前活跃的事务号
    4.  快照读取就是在key(包括当前的版本号）上从后往前scan，忽略掉第三步记录的活跃的事务号（这样可以避免脏读）
    5.  写入时要检测有没有写写冲突，可以从活跃的最小事务号开始，从前往后scan，看有没有事务对相同的key有写入，有的话就返回错误
    6.  每个update都需要记录，用于回滚。

# 分布式系统

## 分布式事务

percolator和Omid比较常见，它们都实现了SI(快照隔离)，上述事务的实现会有两个版本号：startTS和commitTS，有如下规则

1.  startTS一般就是事务开始时分配的事务号
2.  有了startTS之后，客户端就可以获得snapshot(Snapshot必须要能读到所有commitTS\<startTS的数据)，这样客户端可以从snapshot读数据，
    并且可以计算出哪些数据需要写入(writeSet)，也就是说主要的计算在客户端完成
3.  客户端完成计算后接着获得commitTS, 然后进入提交流程，此时要检测写写冲突

写写冲突检测，符合下面两个条件意味着冲突：

1.  T1和T2的\[startTS, commitTS\]有重叠
2.  T1和T2的writeSet有重叠

### percolator

tikv中的实现，将rocksdb分成3个column family：

1.  CF_DEFAULT: (key, start_ts) -\> value
2.  CF_LOCK: key -\> lock_info
3.  CF_WRITE: (key, commit_ts) -\>
    write_info

详细流程如下

1.  客户端从TSO获得startTS，然后进行本地计算得到要写入的rows，快照读取的流程如下:
      - 读取key的lock_info,
        如果存在并且lock_info中startts小于当前的startts,
        这时候需要重试，因为可能存在write too late的情况，
        也就是说一个事务已经获得了commit_ts,
        并且`commit_ts< start_ts`,
        但是因为种种原因它目前还没有提交，所以目前读不到该事务的数据，但是
        快照读要求读到所有commit_ts\<start_ts的事务的数据，所以这种情况只能等待该事务commit后，也就是重试。
      - 从CF_WRITE中读取committs\<start_ts的writeinfo
      - 从 write_info 中拿到 start_ts
      - 从CF_DEFAULT中读取上一步的startts为版本的数据。
2.  Prewrite阶段：从要写入的rows中选一个primaryRow，剩下的都是secondary row
    1.  primaryRow加锁：在CF_LOCK中为primaryRow的key写入lockinfo(内容为startTS)，并且要做冲突检测，一方面是是否有其它事务已加锁该row，
        同时要检测`[startTS, +inf]`范围有没有数据,
        如果没有冲突就在CF_DEFAULT中把数据写入（数据的版本是startTS）
    2.  secondary
        row加锁，lock_info的内容是startTS以及primaryRow的信息。冲突检测是一样的，加锁成功就把数据写入
3.  Commit阶段
    1.  从TSO获得commit_ts
    2.  删掉primaryRow的锁，同时在CF_WRITE中为primaryRow的key写入writeinfo(版本号为commit_ts,
        内容为start_ts).
    3.  为secondary rows重复第2步

crash 恢复 如果某个事务T1读一行数据发现有锁，那么意味着存在事务T0，T0的状态如下：

1.  根据lock_info的内容查找primaryRow，如果primaryRow的对应版本无锁，并且write_info内容正常，那么T0事务已提交，所以去掉锁，更新write_info
2.  如果primaryRow的锁消失，并且没有write_info,或者primaryRow干脆不存在，那么T0事务回滚了，那么直接删掉这一行就好
3.  如果primaryRow的锁的过去很久了，那么该事务在commit或者rollback之前就crash了，那么直接回滚事务T0
4.  其它情况都认为T0事务正在执行，所以等待T0 commit或者rollback， 然后重启T1

### Omid

## 2PC

2PC协议主要是用于分布式事务，用于保证作用于多节点的的操作的原子性

# 分布式一致性算法

分布式一致性算法是用于保证多副本的一致性。主要用于多节点复制数据

## raft

主要拆解成4个问题：

### 选主

3个角色：leader, follower, canidate follower一段时间没收到心跳就会进入选主：
1.  Term+1变为candidate， 并且给自己投一票，接着给集群其它节点发送RequestVote RPC.
2.  其它节点收到RequestVote，如果Term高于自身的Term，那么修改自身的Term，并且投一票，记住一个Node的一个Term只允许投一票
3.  如果收到半数节点同意RequestVote，那么成为leader

边界问题：

1.  网络分裂，如果follower处在leader的不同的一边，为了避免一定无法成功的选主，可以在follower的心跳超时时先试着联系集群半数的节点，只有连接成功才进入选主流程
2.  为了避免选主票数分裂，这里需要对选主超时做一下随机化。

<!-- end list -->

### 日志复制

要保证日志一致性，需要满足下面两点：

1.  日志只能从leader流向follower
2.  选主时必须保证新leader有最新的日志

<!-- end list -->

### 日志安全性
### 配置变更
一定要用两阶段变更，因为如果直接变更可能选出两个leader，比如从3个节点变成5个节点，某个时间点1 2是旧配置(认为集群只有三个节点)，3
4 5时新配置(认为集群有5个节点)， 那么1 2可以选出一个leader，3 4 5也可以选出一个leader。

## Paxos

# 存储引擎

1.  读放大：每一次读需要多少磁盘IO（disk read)
2.  写放大：当只需要写入1byte时，存储系统实际写入了n byte,那么写入放大就是n
3.  空间放大：实际占用的磁盘空间与实际数据空间的比值，主要时一些过期数据的影响

## B+ Tree

## Append-only BTree

## LSM

### LevelDB

1.  log file
    
    Log file format
    
    ``` example
          +-----+-------------+--+----+----------+------+-- ... ----+
    File  | r0  |        r1   |P | r2 |    r3    |  r4  |           |
          +-----+-------------+--+----+----------+------+-- ... ----+
          <--- kBlockSize ------>|<-- kBlockSize ------>|
    
     rn = variable size records
     P = Padding
     kBlolckSize = 32kb
    ```
    
    Record format
    
    ``` example
    +---------+-----------+-----------+----------------+--- ... ---+
    |CRC (4B) | Size (2B) | Type (1B) | Log number (4B)| Payload   |
    +---------+-----------+-----------+----------------+--- ... ---+
    Same as above, with the addition of
    Log number = 32bit log file number, so that we can distinguish between
    records written by the most recent log writer vs a previous one.
    ```

2.  table file
    
    ``` example
    <beginning_of_file>
    [data block 1]
    [data block 2]
    ...
    [data block N]
    [meta block 1]
    ...
    [meta block K]
    [metaindex block]
    [index block]
    [Footer]        (fixed size; starts at file_size - sizeof(Footer))
    <end_of_file>
    ```
    
    1.  data block: 存储key，value数据，以key排序存储
    2.  meta block：一些元数据，比如filter，stat等等
    3.  metaindex block: 存储meta block的(offset, size), 每一个meta
        block都有一条记录
    4.  index block： 存储data block的（offset，size），每一个data block都有一条记录

## Merge Tree

这是clickhouse中所使用的列存储引擎
