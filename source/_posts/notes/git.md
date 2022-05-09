---
title: git笔记
date: 2010-02-14 11:54:17
tags:
- git
categories:
- notes
---
# 基本使用

## git的基本命令

  - 安装：sudo apt-get install git git-uid
  - `init`: git init（工作目录运行，会生成一个.git目录，里面会存放索引数据）
  - `add`: 将文件添加到暂存区
    1.  `git add`: 添加文件
    2.  `git add .`: 添加所有
  - `commit`: 提交到版本库，一般调用add后只是在暂存区，只有调用commit才会提交到git版本库
    1.  `git commit -m "comment"`:
    2.  `git commit -a`: 提交所有
    3.  `git commit --amend`: 可以重新提交上一次commit,这在上一次提交遗漏了某些文件 时非常有用.
  - clone: git clone 只要有.git目录，那么就可以调用该命令来得到git管理的 所有文件 eg: git clone
    test.git dest\\\_dir/ git—-\>dest\\\_dir git clone 账户@IP:工作树路径 你的目录
    远程github
  - pull: git pull（将远程分支拉到本地，然后和本地分支合并）
  - push： git push（将本地分支推送远程仓库）
  - `branch`: 创建分支或者查看有哪些分支
    1.  `git branch`: 查看有哪些分支
    2.  `git branch <branch>` 创建分支
    3.  `git branch -d <branch>` 删除分支,如果有commit没有合并,那么会拒绝删除
    4.  `git branch -D <branch>`: 和上一个类似,但是会强制删除
  - `merge`:将制定分支与当前分支合并(git merge TmpBranch 将TmpBranch与当前分支合并）
  - `checkout`:切换到分支, tag或者撤销工作目录的修改
    1.  `git checkout <branch/tag>`: 切换到分支或者tag
    2.  `git checkout -- <file>`: 撤销工作目录\<file\>的修改,实际上就是用暂存区的文件 替换工作目录
  - `diff`: 比较工作区与暂存区或者暂存区与版本库的差异
    1.  `git diff`:工作区与暂存区的diff
    2.  `git diff --staged`: 暂存区与版本库的diff
  - `reset`:重置, `<commit>` 如果不指定那么就为 `HEAD`,可以使用 `HEAD^/HEAD~1` (父 提交),
    `HEAD^^/HEAD~2` (父父提交)等类似的语法指定相对于HEAD的偏移.
    1.  `git reset <commit> <file>`: 不修改引用,但是撤销对文件的暂存,也就是用版本库
        中的文件替换暂存区中的文件, 工作目录的文件不会变动
    2.  `git reset --hard <commit>`: 将引用设置为commit, 同时用commit的文件替换暂存
        区和工作目录. **慎用**.
    3.  `git reset --soft <commit>`: 只将引用设置为commit, 不修改暂存区和工作目录
    4.  `git reset --mixed <commit>`: 将引用设置为commit,同时替换暂存区,但是不替换 工作目录
  - `blame`: 用来确定文件的每一行都是谁修改的

## git一般的工作过程是：

1.  git pull(将远程分支与本地分支合并)
2.  git log（查看其他成员的更改）
3.  git branch (建一个本地分支）
4.  git checkout（进入本地分支）
5.  在该本地分支上进行修改
6.  git checkout（回到主分支）
7.  git merge （将分支上所做的工作合并到主分支）
8.  git branch -d （删除今天创建的分支）
9.  git pull（将远程仓库合并到本地仓库,很重要，因为今天可能别人修改了远程仓库）
10. git push （将本地仓库合并到远程仓库）

# git的原理

## 工作区(work tree)

就是你本地的工作目录树

  - git add :将工作区的文件提交到暂存区,暂存区的目录树会更新

## git的暂存区(index)

暂存区可以看作一个目录树，暂存区的目录树和版本库的目录树是不同的，.git目录有一个index文件，这个文件包含了
git所管理的目录中所有文件的大小以及修改时间，每一次你调用git status或者git
diff这类命令时，git都会比较工作
区的文件与该index文件中记录的文件的差异，如果发现文件的大小已经改变，那么工作区的文件肯定已经改变，如果发
现文件的修改时间已改变，那么git会读取工作区中的文件与暂存区的文件的内容进行比较，如果内容相同，那么git会修
改index文件的相应条目的文件时间(因为虽然时间改变了，但文件内容没有改变)，这就是基本的工作流程了

  - git commit: 将暂存区的文件提交到版本库
  - git reset HEAD : 暂存区的目录树会被当前分支的版本库目录树替换,所以任何未提交的更新都会丢弃,但工作区不受 影响
  - git checkout 或者 git checkout – \<file\>
    :用暂存区的全部或者部分文件来替换工作区的文件,所以这个操作会清
    除没有添加到暂存区的工作区改动, **危险操作**
  - git checkout HEAD 或者 git checkout HEAD \<file\> :
    用HEAD指向的版本库分支的全部或者部分文件来替换暂存区与
    工作区的文件,所以这个操作会清除所有未提交的暂存区与工作区改动. **危险操作**
  - git rm –cached \<file\> : 直接删除暂存区中的文件,工作区不受影响
  - git rm \<file\> : 会删除工作区与暂存区的指定文件

## 版本库

由git管理的分支代码的目录树，比如master分支，以及其它你自己创建的分支,当然操作时一般都只是当前分支的目录树

## git diff

  - git diff : 工作区与暂存区比较
  - git diff HEAD : 工作区与HEAD比较
  - git diff –cached : 暂存区与HEAD比较

## HEAD, master

HEAD(.git/HEAD)实际是一个指针,指向的是.git/refs/heads/branch-name文件,
branch-name可以是master或者你自己创 建的分支名,该文件实际指向的是该分支的最新的一次commit,
如果当前是master分支,那么这几个文件的内容应该是这 样:

  - .git/HEAD : ".git/refs/heads/master" 如果用git
    checkout改变分支,那么master会变成相应的分支名,所以HEAD可
    以认为一直指向当前分支
  - .git/refs/heads/master: 235fd887b9f85d44ce94e8d733b8814509c1d4e8
    (master分支最新一次提交的id),通过 git reset
    –\[hard|soft|mixed\]可以重置该文件到指定的commit
    id,那么在你指定的commit id之后的提交就都会丢失.

# git github

1.  git remote add remoteName remoteUrl eg: git remote add origin
    git@github.com:youName/github-example.git
    只第一次新建了仓库时才需要运行，实际上是将本地的仓库与远程的仓库关联起来
    如果出错可以使用命令 git remote rm origin
2.  git push origin master(将本地的仓库推送到远端）
    如果报错那么极有可能是远端仓库被别人提交过了，所以要先pull，与本地合并然后在提交
    git pull origin master
3.  git pull -u origin master origin是远程分支的名字，master是本地分支的名字
4.  子模块
      - 添加子模块: `git submodule add repo-url local-path`
      - clone子模块: `git clone --recursive repo-url`,
        如果不指定recursive，那么clone后需要初始化 `git submodule
        update --init --recursive`
      - 更新： `git submodule update`
      - 删除： ubmodule的删除稍微麻烦点：首先，要在“.gitmodules”文件中删除相应配置信息。然后，执行“git rm
        –cached ”命令将子模块所在的文件从git中删除。
5.  fork后合并原仓库的改变 情景如下：我在github
    fork一个仓库（https://github.com/purcell/emacs.d.git ）到我的的账户
    （https://github.com/yuyang0/emacs.d.git ）然后我将这个fork仓库clone到本地，现在
    我要合并原仓库的改变。。 first: git remote add purcell
    <https://github.com/purcell/emacs.d.git> next: git pull purcell
    master (git fetch purcell, git merge purcell/master) next: git push
    origin master

# gitignore syntax

  - "\#" ： 注释

  - ！ : 取反，即匹配模式的文件不忽略

  - / ： /结尾被当作目录，该目录的所有文件忽略

  - / ： /开头则只忽略工作树根目录开始匹配eg：core/TODO与/core/TODO不
    同，前者只要文件路径包含core/TODO（如doc/core/TODO）即匹配，
    而后者则只匹配工作树根目录下的core文件夹的TODO文件

  - glob模式：glob模式不同于正则表达式，等同于shell模式下的方式
    
    \*：匹配任意字符  
    ?: 匹配任意单个字符  
    \[abcd\]: 匹配a b c d中的一个  
    \[a-z\]: 匹配a-z之间的一个字符
