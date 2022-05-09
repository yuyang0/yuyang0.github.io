---
title: 数理逻辑
date: 2013-02-12 12:54:17
tags:
- math
categories:
- notes
math: true
---
# 形式系统

## concepts

### Consistency

形式系统的一致性就是说系统没有矛盾,从语法上说, P 与 ¬ P 不能在系统中都可证, 从语义上说存在一种解释满足所有的定理,也就是说使定理为真(也就是真陈述), 如果至少有一个经解释后的定理是假陈述,那么形式系统就出现了不一致性. 上述的一致性实际是形式系统(带上解释)同外部世界的一致性, 但是还有一种内部一致性说的是 系统中的定理解释之后应该是相容的,这种情况下并不是说一定要定理是真陈述, 只要 这些陈述能相容就可以了。

一致性不单是形式系统的性质,它还依赖于对形式系统提出的解释, 有可能一种解释下 形式系统是不一致的, 而另一种解释下形式系统就是一致的, 同理,不一致性也不是任何形式系统的固有性质,有可能只是没有为形式系统找到好的解释。

假设集合A是形式系统的定理集, 集合B是外部系统中真陈述翻译成形式系统的wff后组 成的集合, 那么一致性会确保A是B的子集.
而完全性则确保B是A的子集, 所以如果一 个形式系统既是一致的又时完全的,那么集合A与集合B是相等的.

### Completeness

**完全性**: 所有真的(在某个想象的世界里)且可表示成系统的 wff 的陈述都是该形式 系统的定理.

**不完全性**:形式系统的定理集之外有真陈述, 也就是说非定理集中存在至少一个 wff 被解释之后是真陈述

### 判定

如果 P 与 ¬ P 都不是形式系统的定理, 那么 P 就在该形式系统内是不可判定的

### Decision procedure

形式系统判定过程用来确定一个 wff 是否是一个定理. 形式系统的公理必须拥有判定 过程, 也就是说你必须保证开始时是正确的.
一个形式系统可以没有判定过程. 一般 来说如果形式系统的推导规则只有加长规则, 那么该形式系统拥有判定过程.

集合的判定过程是用来确定一个元素是否属于一个集合, 如果一个集合拥有一个能行 的判定过程,那么该集合是可判定集合(decidable set)

### 同构

两个复杂结构之间不同部分可以相互映射, 那么这就是一种同构. 对于形式系统来说, 一般是给系统某种 **解释**, 这种情况下形式系统中的每一个符号都与一个词对应, 而 且从上层来说形式系统中的定理解释之后应该是真陈述. 比如pq系统, 可以这样解释:

``` example
q     <=>    等于
p     <=>    加
-     <=>    1
--    <=>    2
---   <=>    3
      ...
```

### 递归, 递归可枚举

二者的关系就像图形(正空间)和它的衬底(负空间).

- 递归可枚举(r.e.): 又称为部分递归集, 任何形式系统的定理集都是递归可枚举集, 更一般的说,
该集合的元素都可以通过推理规则从起点(公理)生成出来.(正空间是 图形).
- 递归: 如果一个集合与它的补集都是递归可枚举的, 那么该集合是递归的(一幅画的 正空间与负空间都是图形 ).

存在非递归的递归可枚举集

### logic system

一个形式系统外加一个语义, 也就是给形式系统中形式语言的句子赋予真值

## Definition

Formal systems in mathmatics consist of the following elements:

1.  A finite set of **symbols** (ie. the alphabet), that can be used to
    construct formulas
2.  A **grammar** which tells well-formed formulas(abbreviated **wff**)
    are constructed out of the symbols in the alphabet, it is usually
    required that there is a decision procedure for deciding whether a
    formula is well formmed or not
3.  A set of **axioms** or axiom schemate each axiom will be a wff.
4.  A set of **inference rules**.

规则2用来生成合式公式(良构公式), 规则3是公理,规则4用来推导系统中的定理. 要注 意这里说的定理和通常意义的定理不同.

## examples

### pq形式系统

1.  symbols: p, q, -

2.  grammar: 一组短杠开头紧接着一个q, 又是一组短杠, 在接着是p, 最后以一组短杠 结尾, 该语法生成的字符串叫
    **合式公式** 也叫 **良构串**.
    
    ``` example
    str ::= SqSpS
    S   ::= --*
    ```

3.  axiom schemate: 如果x仅由一组短杠组成,那么x-qxp- 是一个公理

4.  inference rules: 如果x,y与z是只包含短杠的特定字符串,并且假设 xqypz 是一个 定理, 那么 x-qypz-
    也是一个定理.

### Propositon logic

1.  symbols: (, ), ¬, ∧, ∨, →, ↔, A<sub>1</sub>, A<sub>2</sub>,…
    A<sub>n</sub>.

2.  grammar:
    
    str ::= A<sub>1</sub>  
          |= A<sub>2</sub>  
        …  
          |= A<sub>n</sub>  
          |= (str ∧ str)  
          |= (str ∨ str)  
          |= (¬ str)  
          |= (str → str)  
          |= (str ↔ str)

3.  axiom: 没有公理

4.  inference rules:
    
      - 如果 x 是定理的前提下可以推出 y 是定理, 那么 $x \rightarrow y$ 是定 理.(**演绎定理**).
        这条规则不像其它规则一样, 它不需要依赖定理, 所以它没 有使用\`\`如果xxx是定理,那么XXX是定理''的句式,
        所以它可以"凭空"造出定理, 这也是为什么它可以不需要公理. 比如:
        $P \rightarrow \neg\neg P$ 根据上 述规则就是一个定理, 因为P为定理的前提下可以推出 ¬¬ P
        是定理. 但 是要注意即便P不是定理的情况下, $P \rightarrow \neg\neg P$ 仍然是定理.
      - **分离规则**: 如果 x 与 x → y 都是定理, 那么 y 是定理.
      - **联接规则**: 如果x, y都是定理, 那么 $x \land y$ 也是定理
      - **分割规则**: 如果 $x \land y$ 是定理, 那么x, y都是定理.
      - **双重否定规则**: ¬¬ 可以被删除 (负负得正)
      - **易位规则**: $x \rightarrow y$ 与 $\neg y \rightarrow \neg x$
        是可互换的.
      - **德.摩根规则**: $\neg x \land \neg y$ 与 $\neg(x \lor y)$ 是可互换的.
      - **思维陀螺规则**: $x \lor y$ 与 $\neg x \rightarrow y$ 是可互换的
    
    可互换意味着二者重言等价.

### 数论系统

1.  symbols: 0, S, =, +, ×, ¬, ∧, ∨, ∀, ∃, a, a', a'' …

2.  grammar:
    
    **number** ::= 0  
                 = S **number**  
    **variable** ::= a, a', a'', …  
    **term** ::= **variable**  
                 = **number**  
                 = S **term**  
                 = **term** + **term**  
                 = **term**  × \*term\*  
    **atom** ::= **term** = **term**  
    **formula** ::= **atom**  
                 = ¬\*formula\*  
                 = **formula** ∧ Formula  
                 = **formula**  ∨ \*formula\*  
                 = ∀ u: **formula** (u is a free variable of formula)  
                 = ∃ u: **formula** (u is a free variable of formula)

3.  axioms:
    
    1.  ∀ a: ¬ Sa = 0
    2.  ∀ a: (a + 0) = a
    3.  ∀ a: ∀ b: (a + Sb) = S(a + b)
    4.  ∀ a: (a × 0) = 0
    5.  ∀ a: ∀ b:(a × Sb) = ((a × b) + a)

4.  inference rules: 命题演算中推理规则都是数论形式系统的规则, 除此之外,还有如下规则:
    
    1.  特称规则: 如果 ∀ u: x 是一个定理, 那么x也是一个定理, 并且对x中 的u进行的任何替换都是定理(u是x中自由变量,
        替换u的项不能包括已在x中量 化的变量, 这样会产生变量捕捉)
    2.  概括规则: 和特称规则相反
    3.  存在规则
    4.  等号规则
          - 如果 s=r 是定理, 那么 r=s 也是定理
          - 如果 r=s 与 s=t 都是定理, 那么 r=t 是定理
    5.  后继规则
          - 如果 r=t 是定理, 那么 Sr=St 是定理
          - 如果 Sr=St是定理, 那么 r=t 是定理
    6.  归纳规则 如果 ∀ u:\<X{u} → X{Su/u}\> 以及 X{u/0} 都是定理, 那么 ∀ u: X{u}是定理.

## 哥德尔不完全性定理

哥德尔的不完全性定理的证明实际就是构造一个类似说谎者悖论的公式
$$
     \begin{equation}
       \neg \ \exists a: \exists a': \textrm{TNT-PROOF-PAIR}
\{a, a'\} \land \textrm{ARITHMOQUINE}\{a'', a'\}
     \end{equation}
$$
# Propositon logic

## concepts

1.  命题符号: $A_1, A_2, \dots A_n$ are propositon symbols
2.  逻辑联接符: 命题逻辑中为 $\lnot, \lor, \land, \rightarrow, \leftrightarrow$
3.  参数(非逻辑符号): 命题逻辑中,所有命题符号是参数, 参数的翻译是可变的
4.  逻辑符号: 命题逻辑中, 命题联接符以及括号是逻辑符号, 它们的翻译是不变的.
5.  well-formed formula(合式公式): 根据形式语言的语法规则生成的符号串
6.  满足(satisfy): 一个真值指派 v 满足 $\varphi$ 当且仅当
    $\overset{-}{v}(\varphi)=T$, 也就是使得合式公式为真.
7.  **紧致性定理**: 如果 *Σ* 是合式公式的无限集合, 如果对于 *Σ* 的任意子集 $\Sigma_0$ 都存在一个真值指派满足 $\Sigma_0$ 的每一个公式, 那么存在一个真 值指派满足 *Σ* 中的所有公式.
8.  形式语言: (1) 符号集. (2) 语法规则,用来生成wff. (3) 一种解释或翻译

## 形式语言

### 符号集

命题符号(A<sub>1</sub>, A<sub>2</sub>, …, A<sub>n</sub>), 左右括号, 逻辑联接符(¬, ∧,
∨, →, ↔)

### 语法规则(wff)

语法规则是归纳定义的合式公式集的.

1.  every propositon symbol is well-formed formula
2.  if *α* and *β* are well-formed formula, then $(\lnot \alpha),
      (\alpha \land \beta), (\alpha \lor \beta), (\alpha \rightarrow \beta),
      (\alpha \leftrightarrow \beta)$ are also well-formed formula.

### 解释

逻辑符号的解释是固定的:

  - ¬(非),
  - ∧(且)
  - ∨(或)
  - →(如果…那么)
  - ↔(等价)

参数也就是命题符号的解释是可变的,实际上可以说是任意的.

## 真值指派

1.  S 是一个命题符号集合,比如 {A<sub>1</sub>, A<sub>2</sub> …}
2.  v 是一个真值指派,也就是说为 S 中的每一个命题符号指定一个真值.
3.  $\overset{-}{S}$ 是由 S 中命题符号所组成的合式公式所组成的集合
4.  $\overset{-}{v}$ 为 $\overset{-}{S}$ 中每一个合式公式指定一个真值.
$$
    \begin{align}
        v : S \rightarrow \{T, F\}   \\
        \overset{-}{v} :\overset{-}{S} \rightarrow \{T, F\}    \\
    \end{align}
$$
$\overset{-}v$ 的存在性可以由递归定理来证明, 因为5个逻辑联接符函数是自由生成 的, 所以必然可以将 v 由 S 扩展到
$\overset{-}S$

| *α* | *β* | ($\lnot$ *α*) | (*α* ∧ *β*) | (*α* ∨ *β*) | (*α* → *β*) | (*α* ↔ *β*) |
| --- | --- | --------------- | ----------- | ----------- | ----------- | ----------- |
| T   | T   | F               | T           | T           | T           | T           |
| T   | F   | F               | F           | T           | F           | F           |
| F   | T   | T               | F           | T           | T           | F           |
| F   | F   | T               | F           | F           | T           | T           |

1.  ∨, ∧, $\lnot$ 或且非
2.  →(imply, if..then…) 代表一个承诺, 以 A → B 为例 只 有A 为 T 且 B 为 F时A → B 才为F,
    其它情况 A → B 都为 T.
3.  ↔ 可以认为是等价关系, 要 A ↔ B 为 T, 那么只有 A 与 B有相同的真值.

## 重言蕴含

**definition**: $\Sigma \vDash \tau$ 当且仅当满足 *Σ* 中每个合式公式的指派 也满足 *τ* .

重言蕴含实际反应了一种直觉: 一个结论可以有一些假设条件推出, 只有这些假设条件 都为真时, 结论才为真.

如果 *Σ* 是 ∅ 那么 $\vDash \tau$ 意味着任意真值指派都满足 *τ* 也 就是说 *τ* 恒为真.此时 *τ* 也叫
**重言式**.

如果 *Σ* 是包含一个元素*σ* 那么 $\{\sigma \}\vDash \tau$ 可以写作
$\sigma \vDash \tau$, 如果 $\sigma \vDash \tau$ 且
$\tau \vDash \sigma$ ,那 么意味着*τ* 与 *σ*\*重言等价\* 记作:
$\sigma \vDash \Dashv \tau$. 这也意味着 $\sigma \leftrightarrow \tau$
是重言式.一般在进行合式公式化简时,就是将 一个公式转化为它的一个较简单的重言等价形式(因为重言等价意味这二者可以相互替 换).

**几个结论**:

1.  重言蕴含的基本意义
    $$
      \begin{equation*}
          \Sigma;\alpha \vDash \beta \quad \textrm{iff} \quad \Sigma \vDash (\alpha \rightarrow \beta)
    \end{equation*}
    $$
2.  *α*, *β* 是重言等价的 **iff** $\vDash (\alpha \leftrightarrow \beta)$

3.  对于任意的合式公式B
    $$
        \begin{equation*}
          \{A, \neg B\} \vDash B
        \end{equation*}
    $$
    上式实际就是 $A \rightarrow (\neg A) \rightarrow B$, 很显然是重言式.

### 典型的重言式

1.  分配律
    $$
         \begin{align}
           (A \land (B \lor C))  \leftrightarrow (A \land B) \lor (A \land C) \\
           (A \lor (B \land C))  \leftrightarrow (A \lor B) \land (A \lor C)
         \end{align}
    $$
2.  否定
    $$
         \begin{align}
           &(\neg \neg A) \leftrightarrow A  \\
           &\neg (A \rightarrow B) \leftrightarrow (A \land (\neg B)) \\
           &\neg (A \leftrightarrow B) \leftrightarrow (A \land (\neg B)) \lor (\neg A \land B)
         \end{align}
    $$
3.  德.摩根
    $$
         \begin{align}
           (\neg (A \land B)) \leftrightarrow (\neg A) \lor (\neg B)  \\
           (\neg (A \lor B)) \leftrightarrow (\neg A) \land (\neg B)
         \end{align}
    $$
4.  其它
    $$
         \begin{align}
           &(A \lor (\neg A))  \\
           &\neg(A \land (\neg A))  \\
           &(P \rightarrow Q) \leftrightarrow (\neg Q \rightarrow \neg P)   \\
           &((A \land B) \rightarrow C)  \leftrightarrow (A \rightarrow (B \rightarrow C))
         \end{align}
    $$
    最后一个可以这样理解:
    
    ``` example
    if A {
        if B{
            C
        }
    }
    ```
    
    等价于
    
    ``` example
    if A and B{
        C
    }
    ```
    
    也就是说A,B是条件, C是结论.

## 归纳与递归

### concepts

1.  **自由生成**: 称 C 是 B 在 f 与 g作用下自由生成的,当且仅当除满足生成条件外, f 和 g 在 C上的限制
    f<sub>C</sub> g<sub>C</sub> 必须满足以下条件:
    1.  f<sub>C</sub> 与 g<sub>C</sub> 是一对一的
    2.  f<sub>C</sub> 的值域, g<sub>C</sub> 的值域与 B 两两不相交
2.  **封闭的**: 集合 S 在f与g的作用下是是封闭的当且仅当如果 x, y 是 S的元素,那 么f(x,y) 与 g(x) 也是
    S的元素.
3.  **归纳的**: 如果 S 是归纳的, 那么S满足下面两条性质(B是包含一些基本元素的集合)
      - $B \subseteq S$
      - S 在 f与g作用下封闭
    但是要注意 **一个归纳的集合包含的基本元素是可以比B中元素多的**.这也是为什 么在 **归纳定义** 中要加入"最小"一词的原因.
    举个例子B={0}, f(x)=x+3, 很显 然{0, 3, 6, …..} 是归纳的, 但是{0, 1, 3, 4, 6, 7,
    …}也是归纳的, 这个 集合你可以看出它的基本元素集合是{0, 1}, 这并不违反定义.

### 归纳

归纳实际就是通过集合U的某些初始元素, 重复的运用几种运算, 可以构造U一个子集, 该子集是包含初始元素并且对运算闭合的最小集合.

1.  **自上而下定义**: $C^*$ 是U的所有归纳子集的交集
2.  **自下而上定义**: 设 $C_*$ 是 B 中的元素经过有限次的使用 f 和 g 得到的所有 元素的集合,
    临时定义一个构造序列是 U 中元素组成的有限序列 \<x<sub>1</sub>,
    x<sub>2</sub>,…, x<sub>n</sub>\>, 那么对任意$i \leq n$ 它至少满足以下三个条件之一:
    $$
         \begin{align}
           &x_i& \in B          \\
           &x_i& =  f(x_j, x_k) \quad \textrm{j < i and k < i} \\
           &x_i& =  g(x_j)      \qquad \textrm{j < i}
         \end{align}
    $$
    $C_n$ 是所有长度为 n 的构造序列的最后一个元素组成的集合, 那么 $C_1=B$,
    $$
         \begin{align}
           C_1 \subseteq C_2 \subseteq C_3 \subseteq ...
         \end{align}
    $$
    
    $C_* = \bigcup_n C_n$
    
    该定义实际说明了一个重要性质: **归纳集合的每一个元素都可以由该元素之前的 一些元素来构造一个生成序列**
    
    以自然数为例, U 是实数集, B={0}, S(x) = x + 1
    
      - 长度为1的序列: \<0\> (C<sub>1</sub> ={0})
      - 长度为2的序列: \<0, 0\>, \<0, 1\> (C<sub>2</sub> = {0 1})
      - 长度为3的序列: \<0, 0, 0\>, \<0, 0, 1\>, \<0, 1, 1\>, \<0, 1, 2\>
        (C<sub>3</sub> = {0 1 2})

$C^* = C_*$ 也就是说两种定义等价, 一般用 C 来代替二者.

**归纳法则**: 假设C是由B中的元素通过 $\mathcal{F}$ 中的函数生成的, 若 S 是 C的 子集, S包含B且在
$\mathcal{F}$ 中函数的作用下是封闭的, 那么 C=S.

**归纳法**: 归纳法一般用来证明一个归纳定义的集合的每个元素都具有某个性质, 一 般的证明步骤是这样的:

  - 证明对于基本元素是满足该性质的
  - 对于任意的元素a<sub>n</sub>, 那么必有一个构造序列\<a<sub>1</sub>, a<sub>2</sub>,…,
    a<sub>n</sub>\>, 假设对于i\<n, 都有a<sub>i</sub> 满足该性质, 那么在这个前提下来证明
    a<sub>n</sub> 也是满足该性质的.

举个例子, C是由B中的元素通过f,g生成的, 证明 C 满足某种性质:

1.  证明 B 中的元素有该性质
2.  对于任意的元素a<sub>n</sub>, 存在 a<sub>i</sub>, a<sub>j</sub>, 且
    a<sub>n</sub> = f(a<sub>i</sub>, a<sub>j</sub>), 由归纳假设a<sub>i</sub>,
    a<sub>j</sub> 满足该性质, 那么证明 a<sub>n</sub> 也满足该性质
3.  对于任意的元素a<sub>n</sub>, a<sub>n</sub> = g(a<sub>i</sub>), 由归纳假设
    a<sub>i</sub> 满足该性质, 那么证明 a<sub>n</sub> 也满足该性质.

### 递归定理

U 的子集 C 是由 B 在 f 与 g上 **自由生成** 的, 其中:
$$
      \begin{align}
        f: &U \times U& \rightarrow U   \\
        g: &U& \rightarrow U
      \end{align}
$$
V 是集合,函数 F, G 与 f 满足:
$$
      \begin{align}
        f: &B& \rightarrow V   \\
        F: &V \times V& \rightarrow V \\
        G: &V& \rightarrow V
      \end{align}
$$
那么存在唯一的函数
$$
      \begin{align}
        \overset{-}{h}: C \rightarrow V
      \end{align}
$$
使得:

1.  对 B 中的 x, $\overset{-}{h}(x) = h(x)$
2.  对 C 中 x 与 y:
    $$
         \begin{align}
           &\overset{-}{h}(f(x,y)) = F(\overset{-}{h}(x), \overset{-}{h}(y))   \\
           &\overset{-}{h}(g(x)) = G(\overset{-}{h}(x))
         \end{align}
    $$
递归定理说明了, 如果 C 是自由生成的, 那么 B上的函数 $h$, 必定可以扩展到 C上的 函数 $\overset{-}h$

直观的理解: 递归在本质上就是为了计算一个 **大问题** 的答案, 我可以先计算一个 **小问题** 的答案,
然后把小问题的答案组合成大问题的答案.

1.  为了计算 $\overset{-}{h}(f(x, y))$, 我们先计算 $\overset{-}{h}(x)$ 与
    $\overset{-}{h}(y)$ 的答案, 然后通过 F 将这两个小问题的答案组合成大问题 的答案.
2.  为了计算 $\overset{-}{h}(g(x))$, 我们先计算 $\overset{-}{h}(x)$, 然后通 过 G
    将它组合成 大问题的答案.

## 命题联结词

每一个合式公式都可以看做是一个bool 函数, 比如合式公式 *α* 它有 n 个命题 符号, 那么就有一个 n 元 bool
函数与这个合式公式对应.

1.  $\lnot, \land$ 是完备的
    $$
         \begin{align}
           A \lor B            &=& \lnot (\lnot A \land \lnot B)    \\
           A \rightarrow B     &=& \lnot(A \land \lnot B) = \lnot A \lor B  \\
           A \leftrightarrow B &=& (A \land B) \lor (\lnot A \land \lnot B)
         \end{align}
    $$
2.  $\lnot, \lor$ 是完备的
3.  $\lnot, \rightarrow$ 是完备的
    $$
         \begin{align}
           A \land B &=& \lnot(A \rightarrow \lnot B)\\
           A \lor B  &=& \lnot A \rightarrow B        \\
           A \leftrightarrow B &=& (A \rightarrow B) \land (B \rightarrow A)
         \end{align}
    $$

# 一阶逻辑

## concepts

1.  项: 常数符号与变量通过0次或者多次运算 $\mathcal{F}_f$ (由函数符号确定的构 造运算)
    得到的集合.项是指语言中的名词和代词, 一般作为翻译后句子的主语, 如
    果语言中没有函数符号,那么项就是所有常数和变量的集合.实际上项一般指代论域 中的一个元素.
    
    数论形式系统中一般有后继函数(S), 加法(+), 乘法(×), 同时有常量零(0), 以及变量a', a'',a'''…,
    所以项就具有如下形式:
    
    term ::= 0  
           = a', a'', a''' …  
           = S term  
           = term + term  
           = term × term

2.  原子公式: 没有使用逻辑联接符以及量词符号的合式公式, 原子公式相当于命题逻 辑中命题符号的作用.
        $$
            \begin{equation*}
            Pt_1 t_2...t_n
            \end{equation*}
        $$
    其中P是 n元谓词符号, t<sub>n</sub> 是项
    
    在数论系统一般只有一个谓词等于(=), 所以原子公式有如下形式:
    
    atom ::= term = term

3.  合式公式(wff): 原子公式通过0次或者多次使用 $\xi_\lnot, \xi_\rightarrow, \mathcal{Q}_i$ 运算得到的表达式集合. 合式公式也包含原子公式.

4.  自由变量: 和 lambda calculus的 free variable 类似, 量词(∀, ∃, etc) 就相当于lambda calculus中的 *λ* abstraction.
$$
        \begin{align}
          \overset{-}{h}(\xi_\lnot(\alpha)) &=& \overset{-}{h}(\alpha)  \\
          \overset{-}{h}(\xi_\land(\alpha)) &=& \overset{-}{h}(\alpha)  \\
          \overset{-}{h}(\forall v_i \alpha) &=& \overset{-}{h}(\alpha) / \{ v_i \}
        \end{align}
$$

5.  句子: 如果合式公式没有自由变量出现,那么该合式公式就是句子. 之所以叫做句子 是因为它解释后是一个完整的句子(包含主语).
    句子是一个断言,所以它要么为真, 要么为假

6.  谓词: 如果一个合式公式中包含至少一个自由变量,那么该合式公式解释后就是一个 谓词,也就是说是一个不带主语的句子,
    或者它的主语是一个脱离了上下文的代词.就 像下面的句子:
    
    "是一个不带主语的句子"  
    "会是一个反常现象"  
    "他是中国人"
    
    第三个句子中"他"是一个代词, 但是这个代词脱离上下文, 所以你也无法弄清楚这 个代词指代的是什么,
    这和没有主语是等效的.谓词表达的是一种性质. 所以谓词不 同于句子,
    谓词是既不为真, 也不为假, 只有但当你实例化自由变量后, 才能确定 真假,对具体的事物,它可以具有该性质,也可以不具有.

7.  模型(model): 如果 $\vDash_\mathfrak{A} \varphi$, 那么也就是说
    $\mathfrak{A}$ 是 **句子** $\varphi$ 的模型. 注意只是句子, 也就说不能出现自 由变量,
    如果$\mathfrak{A}$ 是句子集 *Σ* 的每一个句子的模型, 那么它也是 *Σ* 的模型

8.  初等类(EC): K 是初等类当且仅当对某个句子 *τ*, K = Mod *τ*

9.  广义初等类($EC_\vartriangle$): K 是广义初等类当且仅当对某个句子集 *Σ*, K = Mod *Σ*

10. 形式证明(演绎): 从 *Σ* 到 *φ* 的一个 **演绎**,

11. 假言推理: 从公式 $\alpha, \alpha \rightarrow \beta$ 可以推出 *β*.
    $$
        \begin{equation*}
           \frac{\alpha, \alpha \rightarrow \beta}{\beta}
         \end{equation*}
    $$
12. 理论(theory): 逻辑蕴含意义下封闭的 **句子** 集合.
    $$

        \begin{equation*}
           T \vDash \sigma \rightarrow \sigma \in T
         \end{equation*}
    $$
    注意仅仅指句子

## 形式语言

### 符号集

一阶语言的符号包括以下两类:

  - 逻辑符号
    1.  括号: (, )
    2.  命题联结符($\lnot, \rightarrow$)
    3.  变量
    4.  等于符号(可选)
  - 参数(随语言的不同而不同)
    1.  量词符号 $\forall$
    2.  谓词符号:对于每一个正整数n,有一个n元谓词符号集(可以为空集)
    3.  常数符号
    4.  函数符号

在定义一个一阶语言时, 首先就要指定符号集, 也就是该一阶语言有哪些谓词符号, 常数符号以及函数符号, 对任何一阶语言, 逻辑符号都是一样的.

### 语法规则

1.  项
    
    **term** ::= **variable**  
                 = **constant**  
                 = F **term** (F 是一个函数符号)
    
    包含变量, 常量, 以及函数符号应用于项得到的符号串

2.  原子公式
    
    **atom** ::= P term (P 是一个谓词符号)

3.  合式公式
    
    **formula** ::= **atom**  
                 = ¬\*formula\*  
                 = **formula**  ∧ \*formula\*  
                 = **formula**  ∨ \*formula\*  
                 = ∀ u: **formula** (u is a free variable of formula)  
                 = ∃ u: **formula** (u is a free variable of formula)
    
    包含原子公式, 以及命题联接符和量词符号组合得到的符号串.

### 解释

一阶语言使用结构(structure)来给语言指定意义, 实际上一个一阶语言的结构就是该 语言的一种解释, 所以结构必须为 ∀, 函数, 谓词,
常量符号指定意义. 形式 上一个一阶语言的结构 $\mathfrak{A}$ 是一个函数,其定义域为参数的集合:

1.  $\mathfrak{A}$ 为全称量词 ∀ 指派一个非空集合 $|\mathfrak{A}|$, 称
    为$\mathfrak{A}$ 的论域(universe)或者是定义域(domain)
2.  $\mathfrak{A}$ 给每一个n元谓词符号指定一个n元关系 $P^\mathfrak{A}
      \subseteq |\mathfrak{A}|^n$
3.  $\mathfrak{A}$ 为每一个常数符号指定论域 $|\mathfrak{A}|$ 中的一个元素
    $c^\mathfrak{A}$
4.  $\mathfrak{A}$ 为每一个n元函数符号 f 指派一个 $|\mathfrak{A}|$ 上的运算:
    $f^\mathfrak{A}: |\mathfrak{A}|^n \rightarrow |\mathfrak{A}|$

有了结构之后,那么我们就可以把一个句子翻译成外部系统的陈述(比如集合论的陈述), 然后在外部系统中检查该句子的真假,
比如定义一个一阶语言,该语言包含:

  - 等号
  - ∈(二元谓词符号)

定义一个结构,论域是所有集合, ∈ 是子集关系, 那么任何该语言的公式都可以通过 该结构翻译成集合论系统中的陈述,
我们可以在集合论中来判断该句子是不是正确的.

结构有时候可以非形式的写成一行,比如下面:
$$
      \begin{equation*}
        \mathfrak{A} = (\mathbb{N}; \le, S, 0)
      \end{equation*}
$$
依次指定了论域, 谓词, 函数, 常数

## 真值

### 满足

1.  $\varphi$ 是语言中的合式公式
2.  $\mathfrak{A}$ 是语言的结构
3.  $s: V \rightarrow |\mathfrak{A}|$ 是从集合到论域的函数,实际就是给合式公
    式中的变量指定一个论域中的元素,实际上该函数的主要作用是用来给自由变量指
    定一个论域中元素,非自由变量不受该函数的影响.

那么定义对于 $\mathfrak{A}$, s 满足 $\varphi$ 的含义(也就是使 $\varphi$ 为 真)是:
$$
      \begin{align}
        \vDash_\mathfrak{A} \varphi[s]
      \end{align}
$$
还有一种表示方法:
$$
      \begin{align}
        \vDash_\mathfrak{A} \varphi \lVert a_1, ..., a_k\rVert
      \end{align}
$$

上面的意思是说 $\varphi$ 中自由出现的变量为 $v_1, ..., v_k$, 那么为这些自由 变量依次指定论域中的元素为
$a_1, ..., a_k$.

1.  项(term)
    
    项一般代表代词或者名词,所以它应该映射到论域中的一个元素
    
    `s` 是一个变量到论域的函数(V 是变量的集合):
    $$
       \begin{align}
         s: V \rightarrow |\mathfrak{A}|
       \end{align}
    $$
    项可以如下定义:
    $$
       \begin{align}
         \overset{-}{s} : T \rightarrow |\mathfrak{A}|
       \end{align}
    $$
    $\overset{-}{s}$ 的递归定义如下:
    
    1.  x 是变量,那么: $\overset{-}{s}(x) = s(x)$
    2.  x 是常数符号, 那么: $\overset{-}{s} = c^\mathfrak{A}$
    3.  如果 t<sub>1</sub>, t<sub>2</sub>… t<sub>n</sub> 是项,
        f是一个n元函数符号,那么:
        $$
          \begin{align}
            \overset{-}{s}(f t_1t_2...t_n) = f^\mathfrak{A}(\overset{-}{s}(t_1), \overset{-}{s}(t_2)...\overset{-}{s}(t_n))
          \end{align}
        $$
2.  原子公式
    
    原子公式是合式公式,所以它应该映射到 {T, F}
    
    1.  对等于符号而言:
        $$
          \begin{align}
            \vDash_\mathfrak{A} \ = t_1t_2[s] \quad \textrm{iff} \quad \overset{-}{s}(t_1)=\overset{-}{s}(t_2)
          \end{align}
        $$ 
    2.  对n元谓词符号 P:
        $$
          \begin{align}
            \vDash_\mathfrak{A} Pt_1 ... t_n[s]  \quad \textrm{iff} \quad \langle \overset{-}{s}(t_1) ... \overset{-}{s}(t_n) \rangle \in P^\mathfrak{A}
          \end{align}
        $$

3.  其它合式公式
    
    1.  原子公式同上
    
    2.  对 $\lnot$ 逻辑符号
        $$
          \begin{align}
            \vDash_\mathfrak{A} \lnot \varphi[s] \quad \textrm{iff} \quad \nvDash_\mathfrak{A} \varphi[s]
          \end{align}
        $$ 
    3.  对 → 逻辑符号而言:
        $$
                  \begin{align}
            \vDash_\mathfrak{A} (\varphi \rightarrow \psi)[s] \quad \textrm{iff} \quad \vDash_\mathfrak{A} \varphi[s] \ \rightarrow \ \vDash_\mathfrak{A}\psi[s]
          \end{align}
        $$
        也就是说, $\nvDash_\mathfrak{A} \varphi[s]$ 或者
        $\vDash_\mathfrak{A}
          \varphi[s] \ \land \ \vDash_\mathfrak{A} \psi[s]$
    
    4.  对 ∀ 量词符号而言:
        $$
          \begin{align}
            \vDash_\mathfrak{A} \forall x \varphi[s] \quad \textrm{iff} \quad \forall d \in |\mathfrak{A}|(\vDash_\mathfrak{A} \varphi[s(x|d)])
          \end{align}
        $$ 
        s(x|d) 是一个函数, 它在x点取值d, 在其它地方取值与s相同,其定义如下:

        $$
          \begin{align}
            s (x|d)(y) = \left\{ 
                \begin{array}{ll}
                d    & \textrm{if y=x} \\
                s(y) & \textrm{if y \ne x}
                \end{array} 
            \right.
          \end{align}
        $$

### 缩写

1.  $\forall x \in X. P(x)$ 等价于
    $\forall x(x \in X \rightarrow P(x))$
2.  $\exists x \in X. P(x)$ 等价于 $\exists x(x \in X \land P(x))$
3.  $\neg \alpha \land \beta$ 等价于 $((\neg \alpha) \land \beta)$
4.  $\forall x \alpha \rightarrow \beta$ 等价于 $((\forall x \alpha)
      \rightarrow \beta)$
5.  $\neg \alpha \land \beta \rightarrow \gamma$ 等价于
    $((\neg \alpha) \land
      \beta) \rightarrow \gamma$
6.  $\alpha \rightarrow \beta \rightarrow \gamma$ 等价于
    $(\alpha \rightarrow
      (\beta \rightarrow \gamma))$

去掉括号时要记住: ∀, ¬ 的优先级是最高的, 相同的联接符是右结合的.

### 一阶语言的例子

  - 某个类中每个对象都有某种属性:
    $$
       \begin{align}
         \forall v(\_ \rightarrow \_)
       \end{align}
    $$
  - 某个类中某些对象有某个属性:
    $$
       \begin{align}
         \exists v(\_ \land \_)
       \end{align}
    $$
    假设A代表是"是苹果", B代表"是烂的", 那么:

  - $\forall x(Ax \rightarrow Bx)$: 所有的苹果都是烂的

  - $\exists x(Ax \land Bx)$ : 有一些苹果是烂的

  - $\forall x(Ax \land By)$ : 任何东西都是苹果并且是烂的(任何东西都是烂苹果).

## 逻辑蕴含

**Definition**: 设 *Γ* 是合式公式的集合, $\varphi$ 是一个合式公式,那么 *Γ* 逻辑蕴含
$\varphi$, 记作 $\Gamma \vDash \varphi$, 当且仅当对于语言每
一个结构$\mathfrak{A}$ 和每个函数 $s: V \rightarrow |\mathfrak{A}|$,
使得 $\mathfrak{A}$ 以 s 满足 *Γ* 的每一个元素, $\mathfrak{A}$ 也以 s 满足
$\varphi$.

### examples

$$
      \begin{align}
        \forall v_1 Q v_1 \vDash Q v_2  \\
        Qv_1 \nvDash \forall v_2 Q v_2   \\
        \forall v_1 Q v_1 \vDash \exists v_2 Q v_2  \\
        \exists x \forall y Pxy \vDash \forall y \exists x Pxy   \\
        \forall y \exists x Pxy \nvDash \exists x \forall y Pxy
      \end{align}
$$

### 几个结论

逻辑蕴含($\Gamma \vDash \varphi$)实际上还是可以看做这种直觉: *Γ* 中的公 式是前提, 而
$\varphi$ 是结果, 只有满足了所有的前提条件, 结果才是可满足的

1.  一个重要的结论:
    $$
        \begin{align}
         \Gamma;\alpha \vDash \varphi \quad \textrm{iff} \quad \Gamma \vDash (\alpha \rightarrow \varphi)
        \end{align}
    $$
    这个结论也有这样的形式:
    $$
         \begin{align}
           \{a_1, a_2, ..., a_n\} \vDash \varphi \quad \textrm{iff} \quad \vDash (a_1 \rightarrow a_2 \rightarrow ... \rightarrow a_n)
         \end{align}
    $$
    上述转换从直观角度不言而喻.

2.  根据紧致性定理, 必存在
    $$
        \begin{align}
           \{\gamma_1, \gamma_2, ..., \gamma_n\} \subseteq \Gamma
         \end{align}
    $$
    使得如果 $\Gamma \vDash \varphi$, 那么:
    $$
         \begin{align}
           \vDash \gamma_1 \rightarrow \gamma_2 \rightarrow ... \rightarrow \gamma_n \rightarrow \varphi
         \end{align}
    $$
## 同态(homomorphism)

从 $\mathfrak{A}$ 到 $\mathfrak{B}$ 的同态意味着两个结构间存在一个函数:
$h: |\mathfrak{A}| \rightarrow |\mathfrak{B}|$, 具有下列性质:

1.  对谓词
    $$
        \begin{align}
          \langle a_1, ..., a_k\rangle \in P^\mathfrak{A} \quad \textrm{iff} \quad
          \langle h(a_1), ..., h(a_k)\rangle \in P^\mathfrak{B}
        \end{align}
    $$
2.  对函数
    $$
        \begin{align}
          h(f^\mathfrak{A}(a_1, ..., a_k)) = f^\mathfrak{B}(h(a_1), ..., h(a_k)))
        \end{align}
    $$
也就是说,两个结构会保持相同的谓词以及函数关系, 如果 h 是一对一的, 那么二者是 **同构** 的.

## 演绎计算

有限集合 *Λ* 是公理集, 通过选定推理规则, 我们可以生成新的公式, 对于公式 集*Γ*, *Γ* 的 **定理** 是指
$\Gamma \cup \Lambda$ 中的公式通过有限次的推 理得到的公式. 如果 $\varphi$ 是 *Γ*
的定理(记作$\Gamma \vdash \varphi$)

**Definition**: 从 *Γ* 到 $\varphi$ 的一个 **演绎** 是一个有限的公式序列
$\langle a_0, ..., a_n \rangle$, 使得 $a_n = \varphi$, 且对每个
$k \le n$, 那么$a_k$ 二者必居其一:

1.  $a_k$ 在 $\Gamma \cup \Lambda$ 中.
2.  $i < k$ 并且 $j < k$, $a_j = a_i \rightarrow a_k$

那么 $\varphi$ 是 *Γ* 的一个定理, 记作 $\Gamma \vdash \varphi$.

从上面的定义可以很容易看出, $\Gamma \cup \Lambda$ 中的每一个公式都是 *Γ* 的定理.

*Γ* 的定理集是归纳定义的:

  - *Γ* 的定理集包含 $\Gamma \cup \Lambda$ 的所有合式公式
  - *Γ* 的定理集在假言推理上闭合

所以在证明*Γ* 的定理集满足某个性质时, 可以使用归纳法:

  - 如果 $\varphi \in \Gamma \cup \Lambda$ 时, 满足该性质(有时候也可以拆成两 步, 对 *Γ*,
    *Λ* 分别考虑)
  - 如果 $\varphi$ 是由 $\psi, \psi \rightarrow \varphi$ 假言推理得到, 那么假 设
    $\psi, \psi \rightarrow \varphi$ 满足该性质, 那么证明 $\varphi$ 也满足 该性质.

### 逻辑公理集

1.  重言式
2.  $\forall x \alpha \rightarrow \alpha_t^x$ (t替换x, 要避免变量捕捉, 也就 是说
    `t` 不能包含 *α* 中已量化的变量)
3.  $\forall x(\alpha \rightarrow \beta) \rightarrow (\forall x \alpha
      \rightarrow \forall x \beta)$
4.  $\alpha \rightarrow \forall x \alpha$ (x 不是*α* 的自由变量)
5.  $x = x$
6.  $(x = y) \rightarrow (\alpha \rightarrow \alpha')$ (a'是将a中x替换为y得
    来的)

从上面可以看出, 公理集是恒为真的.

注意这里的公理是逻辑推理的公理, 它对任何一阶语言都是一样的, 但是对于一个公 理化的数学理论, 该理论自身会有一些公理, 比如数论的公理化系统中就会有数个公理:
$$
      \begin{align}
        &\forall x & Sx \ne 0   \\
        &\forall x & x + 0 = x   \\
        &\forall x \forall y & x + Sy = S(x + y)
      \end{align}
$$
当然数论系统的公理不只上面3个, 这些理论自身的公理就放在 *Γ* 中.

### 原理,定理

1.  $\vDash, \vdash$ 的关系
    $$
         \begin{align}
           \Gamma \vdash \varphi \quad \textrm{iff} \quad \Gamma \cup \Lambda \vDash \varphi
         \end{align}
    $$ 
    **proof**: 正向: 归纳法, 反向: 紧致性定理

2.  **概化定理**:如果 $\Gamma \vdash \varphi$ 且x不在 *Γ* 中任何公式中自由 出现, 那么
    $\Gamma \vdash \forall x \varphi$

3.  **演绎定理**: 如果 $\Gamma;\gamma \vdash \varphi$ 那么
    $\Gamma \vdash (\gamma \rightarrow \varphi)$ (逆定理也成立,
    逆定理本质就是假言推理).

4.  **归纳原理**: 如果 S 是包含 $\Gamma \cup \Lambda$ 的集合, 并且在假言推理下 封闭, 那么 S 包含
    *Λ* 的所有定理.

## 一致性与完备性

1.  逻辑公理是恒为真的.
2.  一致性: 如果 $\Gamma \vdash \varphi$ 那么 $\Gamma \vDash \varphi$ (一致性
    要表明的是演绎计算会得到正确的结论)
3.  完备性: 如果 $\Gamma \vDash \varphi$ 那么 $\Gamma \vdash \varphi$
