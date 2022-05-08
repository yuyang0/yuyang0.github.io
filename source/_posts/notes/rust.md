---
title: rust笔记
date: 2014-02-15 11:54:17
tags:
- rust
- lang
categories:
- notes
---
## slice
String, Array和Vector都可以使用 &v[1..4]这样的语法获得slice,字符串的slice的类型是&str， 其它类型的slice是&[T], 只是名字不同，内部都是一个指针加上长度。

```rust
let ss = String::from("hello world");
let arr = [11, 22, 33];
let v = vec![11, 22, 33];

let ss_slice = &ss[1..];
let ss_slice = &ss_slice[1..] // 等价于 &ss[2..]
let arr_slice = &arr[1..];
let v_slice = &v[1..];

// 这里会隐式调用arr.into_iter(), 所以val是值，并且会获得所有权
// 如果要val为引用，那么必须显式的指定arr.iter()或者arr.iter_mut()
for val in arr {

}
// 默认也会调用arr_slice.into_iter(), 但是这里val是引用(&T), 这是slice的into_iter的正常行为。当然这里也可以明确使用iter()来返回引用。
for val in arr_slice {

}
```
注意：&str不能使用for in 遍历，它没有实现Iterator。可以先调用as_bytes转换成&[u8]

## dot operator
```rust
let s = String::from("hello world");
let ref_s = &&s;  
l = ref_s.len(); // 等价于 (***ref_s).len()
```
在进行方法调用时，不管是多少层都引用会被编译器还原成对应的值，然后根据参数定义(self, &self, &mut self)决定是传值还是传引用。

## Deref
Deref主要是为了智能指针引入的trait，它的行为都是为了服务智能指针，如果不是智能指针不建议实现Deref，除了常见Box，Rc外，String，Vector等都是实现了Deref的。

```rust
pub trait Deref {
    type Target: ?Sized;
    fn deref(&self) -> &Self::Target;
}
```


If `T` implements `Deref<Target = U>`, and x is a value of type `T`, then:

1. In immutable contexts, `*x` (where `T` is neither a reference nor a raw pointer) is equivalent to `*Deref::deref(&x)`.
2. Values of type `&T` are coerced to values of type `&U`
3. `T` implicitly implements all the (immutable) methods of the type `U`.

在需要&str, &[T]的地方之所以能直接传String，Array，Vector的引用就是因为它们实现了Deref，所以根据规则2，它的类型会自动转换


## AsRef
```rust
#[derive(Default)]
struct User {
    email: String,
    age: u8,
}

// obviously
impl AsRef<User> for User {
    fn as_ref(&self) -> &User {
        self
    }
}

enum Privilege {
    // imagine different moderator privileges here
}

#[derive(Default)]
struct Moderator {
    user: User,
    privileges: Vec<Privilege>,
}

// since moderators are just regular users
impl AsRef<User> for Moderator {
    fn as_ref(&self) -> &User {
        &self.user
    }
}

fn takes_user<U: AsRef<User>>(user: U) {
    let user = user.as_ref();
}

fn main() {
    let user = User::default();
    let moderator = Moderator::default();
    
    takes_user(&user);
    takes_user(&moderator); // yay
}
```
takes_user能接受User对象以及Moderator对象，因为它们都实现了AsRef，并且返回了&User, 而takes_user的参数类型是AsRef<User>.
## impl Trait vs dyn Trait
https://www.ncameron.org/blog/dyn-trait-and-impl-trait-in-rust/

简单点说，`impl Trait` 在编译时就确定了具体类型, 它会像泛型参数一样展开生成具体类型的函数副本(单态化)，所以基本没有多余开销（静态分发），但是`dyn Trait` 则不然, 内部实际上是用一个虚拟表来dispatch 函数调用(动态分发）。也就是说一个变量对应多种类型。

优缺点：
1. impl Trait只能存在于函数的签名中，其它地方不能使用，比如你不能用这种语句定义变量：`let v: impl T;`
2. dyn Trait只能使用`&dyn trait`或者 `Box<dyn trait>`这种形式。
3. impl Trait可以使用多个Trait，比如 `impl T1+T2` 但是dyn Trait不行
4. dyn Trait 可以在一个变量中存储多个不同的具体类型，比如一个函数返回 dyn Trait, 那么函数体可以返回多个不同的具体类型，而impl Trait则只能返回相同的具体类型。这主要是因为impl Trait是要展开成具体类型的。
5. 要使用 dyn Trait， 那对应的trait必须是对象安全的(object safe), object safe的条件如下
   1. trait的方法不能返回Self类型
   2. trait不能有泛型参数


```
trait Bar {
    fn bar(&self) -> Vec<Self>;
}

impl Bar for Foo { ... }
impl Bar for Baz { ... }

// 1. 作为函数参数
// 作为函数参数，impl Trait实际就是泛型参数，所以下面两个是等价的
fn f(b1: impl Bar, b2: impl Bar) -> usize
fn f<B1: Bar, B2: Bar>(b1: B1, b2: B2) -> usize

fn f(b: &dyn Bar) -> usize

// 2. 作为函数返回值
// T的类型由f的调用者指定，也就是说调用者知道T的具体类型
fn f<T: Bar>(...) -> T

// impl T 的具体类型由f的函数体推导出来, f的调用者不知道 impl T的具体类型。
// 注意f不能返回不同的具体类型, 比如body中有if语句，那么两个分支返回的具体类型必须一样。
fn f(...) -> impl T

// 可以返回不同的具体类型。
fn f(...) -> Box<dyn Bar>
```

## 智能指针
```rust
fn main() {
    let b = Box::new(vec![String::from("haha")]);
    let rc = Rc::new(vec![String::from("haha")]);
    let rf = RefCell::new(vec![String::from("haha")]);

    // Box只能有一个所有者, 也就是说对于一个指向的数据只能有一个Box有效
    let b1 = b; // 此行以后b就无效了

    // Rc可以有多个所有者, 内部有引用计数
    let rc1 = Rc::clone(&rc); // rc和rc1都是有效的.

    // RefCell可以在不可变的上下文进行修改, 比如要修改对象的某个属性，但是只能获得&self, 无法获得&mut self，那么就可以把对应的属性定义成RefCell.
    // 注意：只能获得1个可变引用或者无限多个不可变引用，规则是一样的，只是这里是运行时检查。
    let rf1 = rf.borrow();
    let rf1_mut = rf.borrow_mut(); // rf1和rf1_mut不能同时存在
}
```

标准库中实现了智能指针的类型：
1. Box<T>
2. Vec<T>, String
3. Rc<T>, Arc<T>
4. Cell<T>, RefCell<T>
5. RwLock, Mutex

RefCell, RwLock, Mutex都实现了内部可变性。

## ref关键字
主要用于pattern match中，如果match后v的类型是T，那么用ref v去match,那么v的类型就是&T。

```rust
let v2 = vec![String::from("str1"), String::from("str2")];
// v的类型是&String，注意这里v2中的元素的所有权仍然会转移出去,
// 你可以认为v2中的元素变成了一个个匿名对象，而v指向匿名对象的引用。
for ref v in v2 {  
    println!("{}", v);
}
// v的类型是&&String.
for ref v in v2.iter() {
    println!("{}", v);
}
// v的类型是String，但是这里很多时候是非法，因为如果里面的元素没实现Copy Trait，那么这里就有编译错误。
for &v in v2.iter() {

}
```
## 生命周期规则
第一条规则是每一个是引用的参数都有它自己的生命周期参数。换句话说就是，有一个引用参数的函数有一个生命周期参数：fn foo<'a>(x: &'a i32)，有两个引用参数的函数有两个不同的生命周期参数，fn foo<'a, 'b>(x: &'a i32, y: &'b i32)，依此类推。

第二条规则是如果只有一个输入生命周期参数，那么它被赋予所有输出生命周期参数：fn foo<'a>(x: &'a i32) -> &'a i32。

第三条规则是如果方法有多个输入生命周期参数并且其中一个参数是 &self 或 &mut self，说明是个对象的方法(method)(译者注： 这里涉及rust的面向对象参见17章), 那么所有输出生命周期参数被赋予 self 的生命周期。第三条规则使得方法更容易读写，因为只需更少的符号。