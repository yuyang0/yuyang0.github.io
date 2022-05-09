---
title: elisp
date: 2010-01-14 11:54:17
tags:
- emacs
- elisp
categories:
- notes
---
# elisp基本语法

## help

  - M-x apropos (find symbol)
  - C-h k(descripte key)
  - C-h f (function)
  - C-h v (variable)

## control flow

1.  `if`
    
    ``` commonlisp
    (if test
    a
    b)
    ;;test is the condition
    ```

2.  `when`
    
    ``` commonlisp
    (when condition a b c)
    ;;; equivalent
    (if condition (progn a b c) nil)
    ```

3.  `unless`
    
    ``` commonlisp
    (unless condition a b c)
    ;;;equvalent
    (if condition nil (progn a b c))
    
    ```

4.  `cond` 和scheme类似:
    
    ``` scheme
    (cond ((numberp x) x)
          ((stringp x) x)
          ((bufferp x)
           (setq temporary-hack x) ; multiple body-forms
           (buffer-name x))        ; in one clause
          ((symbolp x) (symbol-value x)))
    ```

5.  `and` or `not` logical expression: the function `or` works like the
    logical "or" in most languages: if all the arguments are false, it
    return nil, otherwith it will return the value of the last argument
    whose value is non-nil. eg: (or nil nil 3 2 1) return 3 (not t) so
    (if a a b) is identical to (or a b)

6.  `while` **Special Form**: while condition forms…

7.  `dolist` **Macro**: dolist (var list \[result\]) body…
    
    ``` commonlisp
    (defun reverse (list)
      (let (value)
        (dolist (elt list value)
          (setq value (cons elt value)))))
    ```

8.  `dotimes`
    
    ``` commonlisp
    (dotimes (i 100)
                (insert "I will not obey absurd orders\n"))
    ```

## let <span class="tag" data-tag-name="let"><span class="smallcaps">let</span></span>

`let`, `let*`, `letrec` 的作用语法与scheme类似

``` commonlisp
(let ((val1 value1)
      (val2 value2))
  body)
```

## function

``` example
(defun func-name (a b &optional c d &rest e) body)
```

optional代表参数可选, rest代表可变参数, 如果有那么会是一个列表

1.  `apply`: `apply function &rest arguments`
    
    调用函数, `function` 的值是一个symbol(比如你要调用 `list` 函数,那么你应 该传递 'list
    )最后一个参数必须是列表, 这个列表参数会被apply自动拆开,然后 传递给function

2.  `funcall`: 与 `apply` 不同的是,它不会拆开列表参数, 而是直接将列表参数作为一个 参数传递给function
    
    ``` commonlisp
    (setq f 'list)
    (apply f 'a 'b '(1 2))                  ; (a b 1 2)
    (funcall f 'a 'b '(1 2))                ; (a b (1 2))
    ```

## lists

  - `null`: 测试list是否为空
  - `listp`: 是否为list
  - `car`
  - `cdr`
  - `cons`
  - `list`: (list 'a "b" 1) ==\> (a "b" 1)
  - `append`: (append '(a b) '(c d)) ==\> (a b c d)
  - `reverse`: (reverse '(a b c d)) ==\> (d c b a)
  - `nthcdr`: call cdr n times
  - `length`: get the length of the list
  - `mapcar`:
  - `equal`: only test the object's structure and content
  - `eq`: test if the two arguments is the same object(like pointer in
    C)

**以上的函数都不会原地改变list, 它会返回一个新的list作为结果, 下面的函数会 原地改变list**

  - `setcar`: change the car-element of the list
  - `setcdr`: change the cdr-element of the list
  - `push`: 和cons类似,只是会原地改变list
  - `pop`: 和car类似,只是会原地改变list(删除list的第一个元素)
  - `add-to-list`: 和push类似

### Associate List

``` example
((key1 . value1 )
(key2 . value2 )
(keyn . valuenn ))
```

  - `assoc`: 当list类似于关联数组时，用key来寻找value，用equal测试key
  - `assq`: 与assoc相似，只是用eq测试key

### property list

``` commonlisp
(setq alist '(:publish-dir "~/Documents" :base-dir "~/Documents/note"))
(plist-get alist :base-dir)
```

# 常用的函数

## 字符串操作

  - `length`: (length "abc") –\> return 3

  - `substring`: (substring "hello world" 1 3) –\> return "ell"

  - `replace-regexp-in-string`: (replace-regexp-in-string regex replace
    str) –\> 正则替换

  - `string-match`: (string-match regex str) –\> return the index first
    match the regex in str
    
    ``` commonlisp
    (setq mystr "The quick fox jumped quickly.")
    (string-match "\\(qu\\)\\(ick\\)"
                       mystr)   ;=>副作用：将匹配结果存储到match-data中
    (match-string 0 mystr)      ;=>结果为quick，也就是模式匹配的结果
    (match-string 1 mystr)      ;=>结果为qu   ，也就是第一个分组的匹配结果
    (match-string 2 mystr)      ;=>结果为ick  ，也就是第二个分组的匹配结果
    ```

## Regex

1.  用来匹配字符串时 ^匹配字符串的开头, $匹配字符串的结尾, 用来匹配buffer中的 内容时, ^匹配一行的开头, $匹配一行的结尾.
2.  常用的字符串匹配函数是string-match, 常用的buffer 内正则匹配的函数是 `looking-at`
    它会匹配从当前光标开始的内容.
3.  在通常的语言(比如python)的正则表达式实现中, (, ), \[, \], {, }, \\ 等都是特 殊字符, 但是在emacs
    lisp中都是普通的字符,所以如果你要把它们当作特殊的字符, 比如用(,)来分组, 那么你就必须对(,)转义,
    也就是使用\( 与 \), 可是由于 一个普通字符, 所以当你希望它是一个用来转义的特殊字符时, 你必须对
    义(否则), 结果就是\\\(, \\)这样丑陋的东西. 这是 一个设计失误.
4.  M-x re-builder: 可以实时的显示buffer中匹配regex的字符串.

## buffer相关： <span class="tag" data-tag-name="buffer"><span class="smallcaps">buffer</span></span>

  - `buffer-name`: (buffer-name)
  - `buffer-file-name`: (buffer-file-name) –\> full name
  - `save-buffer`: (save-buffer) –\> 保存当前的buffer 的文件
  - `kill-buffer`: (kill-buffer BufferName)
  - `kill-this-buffer`: (kill-this-buffer)
  - `with-current-buffer`: (with-current-buffer BUFFER-OR\_NAME body)
    –\> 将 BUFFER-ORNAME指定的buffer设为当前buffer, body运行完后会恢复原先的buffer, \*
    常用\*

### buffer content相关

  - 增: `insert`
  - 删: `delete-char`, `delete-region` 等等一系列的delet函数
  - 查找: `re-search-forward` 与 `re-search-backward` 最 **常用**
  - 替换: 一般是查找后,使用 `replace-match` 来替换
  - 获取buffer的内容: `buffer-substring`, point上的字符可以用 `char-after` 与
    `char-before` 来获得, point附近的词可以用 `current-word` 得到, 其它类 型的文本可以用
    `thing-at-point`.

### buffer中移动以及光标相关 <span class="tag" data-tag-name="point"><span class="smallcaps">point</span></span>

  - `point` :(point)

  - `point-max` :(point-max) –\> 一般返回buffer-end,但是如果指定了Narrowing,
    那么结果就不同

  - `point-min` :(point-min) –\> 一般返回1,但是如果指定了Narrowing,那么结果就 不同

  - `buffer-end` :(buffer-end)

  - `buffer-size` :(buffer-size)

  - `save-excursion` :(save-excursion body) –\> 可以用来保存当前的point,不管
    body中如何改变point,执行完成后都会回到执行save-excursion之前的point

  - `narrow-to-region` :(narrow-to-region start end) –\>
    Narrowing(将emacs的 文本操作限定在buffer的一个子区域中)

  - `region-beginning` :(region-beginning)

  - `region-end` :(region-end) –\> line

  - `beginning-of-line` :(beginning-of-line)

  - `end-of-line` :(end-of-line)

  - `buffer-substring` :(buffer-substring start end)

  - `goto-char` :(goto-char 293)

  - `forward-char` :(forward-char n)

  - `backward-char` :(backward-char n)

  - `skip-chars-forward` :(skip-chars-forward ""͡)

  - `skip-chars-backward` :(skip-chars-backward ""͡)

  - `forward-line` :(forward-line n)

  - `backward-line` :(backward-line n)

  - `looking-at` :(looking-at regex) ;; return t if text after the point
    match the REGEXP

  - `looking-back` :(looking-back regex)

  - `search-forward` :(search-forward my-str)

  - `search-backward` :(search-backward my-str)

  - `re-search-forward` :(re-search-forward my-regex)

  - `re-search-backward` :(re-search-backward my-regex)

  - `replace-match` :(replace-match)

## file相关 <span class="tag" data-tag-name="file"><span class="smallcaps">file</span></span>

  - `find-file`: (find-file path) ;;open a file
  - `write-file`: (write-file path) ;;save the file
  - `insert-file-contents`: (insert-file-contents path) –\>将指定文件内容插入当前位置
  - `append-to-file`: (append-to-file start-pos end-pos path)
  - `rename-file`: (rename-file old-name new-name)
  - `copy-file`: (copy-file file-name new-name)
  - `delete-file`: (delete-file file-name)
  - `file-name-directory`: (file-name-directory full-path) –\>路径（不包含文件名）
  - `file-name-nondirectory`: (file-name-nondirectory full-path)
    –\>文件名（不包含路径）
  - `file-name-extension`: (file-name-extension file-name) –\>后缀,一般时扩展名
  - `file-name-sans-extension`: (file-name-sans-extension
    "/hello/abc.html") –\>return "/hello/abc",只去掉后缀

## other useful function

  - `save-excursive`: 保存并恢复当前point

  - `interactive`:

  - `thing-at-point`:
    
    ``` commonlisp
    (thing-at-point 'word)
    (thing-at-point 'sexp)
    (thing-at-point 'url)
    ```

  - `bounds-of-thing-at-point`: 和 `thing-at-point` 类似,只是它会返回一个 pair –\>
    (start . end) , 这个pair 的car是左边界,cdr是右边界

  - `current-time-string`: (current-time-string) –\> 返回当前时间字符串

  - `format-time-string`: (format-time-string "%1.%M %p" (current-time))

  - `symbol-name`: 将symbol转换为字符串, eg: (symbol-name 'sym) –\> "sym"

  - `intern`: 将字符串转换为symbol, eg: (intern "sym") –\> 'sym

## **useful variable**

  - `mark-active`: 现在一般用 `region-active-p` 代替
  - `last-command`: 最后一条命令

## useful code snippet

1.  test if a mode(major or minor) is on (flymake-mode as an example)
    
    ``` commonlisp
    (if (and (boundp 'flymake-mode) flymake-mode)
        (message "flymake-mode is on")
      (message "flymake-mode is off"))
    ```

2.  setting keybinding for specified mode **there are two way**, the
    first is **eval-after-load**
    
    ``` commonlisp
    (eval-after-load "org"
      '(progn
         (define-key org-mode-map (kbd "<C-M-return>") 'org-insert-heading-respect-content)
         (define-key org-mode-map (kbd "<M-right>") nil) ; erasing a keybinding.
         (define-key org-mode-map (kbd "<M-left>") nil) ; erasing a keybinding.
         ))
    ```
    
    the second way is **add-hook**
    
    ``` commonlisp
    (defun my-org-settngs ()
        (flyspell-mode 1))
    (add-hook 'org-mode-hook 'my-org-settngs)
    ```
    
    对以上二者的说明:
    
    1.  eval-after-load只运行一次,所以比较适合那些一次性的设置,比如给某个特定的mode设置keymap,它没有当前
        buffer的概念,
        而add-hook这是对每一个打开了该mode的buffer都会运行一次.所以它适合来设置一些和buffer相
        关的设置
    2.  **local-set-key** 实际上也是调用 **define-key** , 其中 **map** 参数是
        **(current-local-map)** 的值. 这个值 一般是由major mode设置的. 所以在给minor
        mode设置keymap时,使用 **define-key** 比较好, 因为可以直接指定 minor mode 的map

# interactive <span class="tag" data-tag-name="interactive"><span class="smallcaps">interactive</span></span>

emacs中交互式运行的命令都需要在函数体的最上方加上(interactive arg-descriptor),
arg-descriptor最后会解析成参数从递给该命令, arg-descriptor 有三种形式

1.  `(interactive)` : `arg-descriptor` 为 `nil`, 该命令没有参数

2.  `(interactive "p\ncZap to char: ")` : `arg-descriptor` 以 `\n` 分割后每一
    部分都会产生一个参数,比如上面的例子,以:
    
    ``` example
    "p"
    "cZap"
    ```
    
    其中每一部分开头的那个字母有特殊的含义:
    
      - p: 接受C-u传入的参数,返回的是整数,默认是1,也就是numeric prefix argument.
        后面不能跟提示字符串,因为参数来自C-u而不是minibuffer
      - c: 从minibuffer中读入一个字符,它后面跟的Zap会作为提示显示在minibuffer 中
    
    除了上面的p, c外还有很多这样的特殊字符,下面是一些常用的,
    这是官方[文档](http://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html#Interactive-Codes)
    , 这是一些单词的含义: **Prompt** (后面可以跟显示在minibuffer中提示字符串), **Existing**
    (file, command… 必须已存在), **Completion** (可以补全) :
    
      - P(uppercase): 和小写p的最大区别是它返回的是raw prefix argument, 默认是 nil,
        后面不能跟提示字符串,因为参数来自C-u而不是minibuffer
      - b: 已存在的buffer name, 可以跟提示字符串, 默认是当前buffer, 可以补全
      - B: 不存在的buffer name, 可以跟提示字符串, 默认是最近使用的buffer
      - f: 已存在的文件名, \[Existing, Completion, Default, Prompt\]
      - F: 不存在的文件名, \[Completion, Default, Prompt\]
      - C(uppercase): 命令名称, \[prompt, Existing, Completion\]

3.  `(interactive (lisp-expression-return-list-argument))` :
    arg-descriptor是 一个elisp表达式,这个表达式应该产生传递给命令的参数,一般会调用 `read-string`
    这样的函数从minibuffer中读入字符串
    
    ``` commonlisp
    (interactive (list
                  (read-string (format "word (%s): " (thing-at-point 'word))
                               nil nil (thing-at-point 'word))))
    
    (interactive
     (let ((string (read-string "Foo: " nil 'my-history)))
       (list (region-beginning) (region-end) string)))
    
    ```
    
    这是 `read-string` 的原型:
    
    ``` example
    read-string prompt &optional initial history default inherit-input-method
    ```

# autoload <span class="tag" data-tag-name="autoload"><span class="smallcaps">autoload</span></span>

使用autoload可以使一个函数或者命令只在调用时才加载进来,这样可以加快emacs的 启动时间,使用autoload有两种方法:

1.  autoload function filename \&optional docstring interactive type
    
    该函数会将function标记为autoload:
    
      - function: 一个symbol,用来代表函数或者宏
      - filename: 一个 `string`, 用来指定加载function的文件,不要带目录名与后缀 名
      - docstring: 文档字符串,指定后可以在没有加载function的情况下看它的文档
      - interactive: bool,如果为 `t` 那么就是一个command,否则就是一个函数
      - type:

2.  使用魔法注释
    
    ``` commonlisp
    ;;;###autoload
    (defun doctor ()
      "Switch to *doctor* buffer and start giving psychotherapy."
      (interactive)
      (switch-to-buffer "*doctor*")
      (doctor-mode))
    ```
    
    那么这个 `doctor` 就是标记为autoload的.实际上上面的注释部分会被自动转换 为 `autoload` 的形式

3.  package.el 中的 `autoload`, 以yasnippet为例, package.el 会根据yasnippet
    中魔法注释创建一个yasnippet-autoloads.el的文件,该文件就是调用 autoload函
    数来自动加载yasnippet.el中指定要自动加载的函数,而
    yasnippet-autoloads.el(所有的package.el管理的包的 -autoloads.el文件)都是是在
    执行(package-initialize)时运行的.这样每一个包需要自动加载的函数就都加载 进来了

# define mode(minor mode or major mode)

## keymap

定义一个mode的时候,通常会用到keymap,keymap规定了该mode下的键映射,可以用如 下的代码来创建一个keymap:

``` commonlisp
(defvar prelude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'prelude-open-with)
    (define-key map (kbd "C-c g") 'prelude-google)
    ;; ... more define-key sexp
    map)
  "Keymap for Prelude mode.")
```

上面就创建了一个名为 prelude-mode-map的keymap

## minor mode

创建一个minor mode,需要经过以下几个步骤:

1.  定义一个变量 xxxx-mode,该变量为 mode varible,如果该 mode已打开,那么这个 变量就为t,否则为nil,
    这个变量一般要设为buffer local varible
2.  定义一个命令 xxxx-mode, 注意命令的名字必须和第一步的变量的名字相同,该命 令的作用就是设置第一步的mode
    varible,当然该命令中也可以添加一些初始化的 操作
3.  添加一个元素到 minor-mode-alist

上面的步骤比较繁琐,这里有一个宏, define-minor-mode可以方便的定义一个minor mode

``` commonlisp
(define-minor-mode prelude-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{prelude-mode-map}"
  :lighter " Pre"
  :keymap prelude-mode-map
  (if prelude-mode
      ;; on start
      (prelude-mode-add-menu)
    ;; on stop
    (prelude-mode-remove-menu)))
```

" Pre"是要在modeline上显示的字符串,prelude-mode-map是该minor-mode的keymap,
下面的body,实际就是 prelude-mode命令了, 可以通过
define-globalized-minor-mode 来把minor
mode设为global,可以参考这里的[代码](https://github.com/bbatsov/prelude/blob/965e5e2fdbc0babbdb3c149e93d9e7662807d0d7/core/prelude-mode.el#L146)
