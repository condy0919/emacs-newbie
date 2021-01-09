# Emacs builtin modes 功能介绍

`Emacs`自带的`mode`功能也比较强大，而一般初学者（比如我）使用`Emacs`时间较短，对
它自身强大的`mode`不了解而错失一些可以提高生产力的工具。

## winner-mode

`winner-mode`是一个全局的`minor mode`，它的主要功能是记录窗体的变动。例如当前有
2 个窗口，然后你关了一个，这时可以通过`winner-undo`来恢复。还可以再`winner-redo`
来撤销刚才的`undo`.

它默认按键绑定为:

<kbd>C-c Left</kbd> `winner-undo`

<kbd>C-c Right</kbd> `winner-redo`

如果不想它绑定在<kbd>C-c</kbd>前缀按键上，可以通过

``` elisp
(setq winner-dont-bind-my-keys nil)
```

来禁止。

建议配置:

```elisp
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))
```

同时，它也可以应用在`ediff`上，恢复由`ediff`导致的窗体变动。

```elisp
(use-package ediff
  :ensure nil
  :hook (ediff-quit . winner-undo)
```

### winner-undo 搭配 transient-map

如果平常有注意观察`text-scale-adjust` (<kbd>C-x C-=</kbd>) 的行为，会发现只需要
按全一次<kbd>C-x C-=</kbd>，之后可以只按`+`、`-`或者`0`来缩放字体。而如果触发了
其他按钮则会退出这个状态，它背后主要依赖`transient-map`机制。

我们可以仿照着写一个`transient-winner-undo`的版本，在需要连续执行`winner-undo`的
时候只需要按一个`u`就好了。

``` elisp
(defun transient-winner-undo ()
  "Transient version of `winner-undo'."
  (interactive)
  (let ((echo-keystrokes nil))
    (winner-undo)
    (message "Winner: [u]ndo [r]edo")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?u] #'winner-undo)
       (define-key map [?r] #'winner-redo)
       map)
     t)))
```

着实方便许多！

## saveplace

`saveplace`记录了上次打开文件时光标停留在第几行、第几列。如果不想每次打开文件都
要再次跳转到上次编辑的位置，这个`mode`可以轻松地应对这种情况。

建议配置:

```elisp
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))
```

## recentf

`recentf`保存了历史以来访问过的文件，开启之后可以通过`recentf-open-files`来打开。
当然如果你使用`ivy`全家桶，也可以通过`counsel-recentf`访问。

建议配置:

``` elisp
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude `("/ssh:"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))
```

默认状态下`recentf`存储的文件是绝对路径，访问一个在家目录下的文件`~/a.cpp`它实际
存储时是用的`/home/user/a.cpp`，可以自定义`recentf-filename-handlers`来简化存储
的路径。需要注意的是，一旦使用了`abbreviate-file-name`，那么`recentf-exclude`变
量内的元素也必须要是`abbrev`过后的字符串，因为 Emacs 是使用`string-prefix-p`来匹
配的。

而`COMMIT_EDITMSG`是`magit`写`commit message`时的文件，记录此文件无意义，故除外。

## hl-line

高亮当前行。

```elisp
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))
```

## newcomment

如果你想要一个足够简单的注释与反注释功能，那么自带的`newcomment`就可以做到。

``` elisp
(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))
```

上方的函数它可以完成:
- 当用户选中区间时，在对应区间上注释或者反注释
- 如果当前行是空的，那么会插入一个注释并且将它对齐 (偷懒，直接调用了`comment-dwim`)
- 其他情况则对当前行注释或者反注释

这个行为也与[evil-nerd-commenter](https://github.com/redguardtoo/evil-nerd-commenter)保持一致。

这里有必要比较一下其他`comment`函数:

1. `comment-dwim`
   - 当用户选中区间时，会在对应区间注释或者反注释
   - 如果当前行是空的，那么会插入一个注释并且将它对齐
   - 如果使用<kbd>C-u</kbd>前缀，会则调用`comment-kill`来删除这个注释
   - 其他情况下则调用`comment-indent`在尾部插入注释并对齐
2. `comment-line`
   - 当用户选中区间时，会在对应区间**再加上下一行**进行注释或者反注释
   - 如果当前行是空的，那么只会跳到下一行不会插入注释
   - 其他情况下则会将当前行注释或者反注释并**跳到下一行**
3. `comment-box` 看例子就行
``` elisp
(defun add (a b)
  (+ a b))

;;;;;;;;;;;;;;;;;;;;;;
;; (defun add (a b) ;;
;;   (+ a b))       ;;
;;;;;;;;;;;;;;;;;;;;;;
```

## hideshow

隐藏、显示结构化数据，如`{ }`里的内容。对于单函数较长的情况比较有用。

建议配置：

```elisp
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))
```

`hideshow`的默认按键前缀为<kbd>C-c @</kbd>，这里放一个默认的按键与经过
`evil-mode`的版本的对比表格:

| 功能               | 原生                   | `evil-mode`   |
|--------------------|------------------------|---------------|
| `hs-hide-block`    | <kbd>C-c @ C-h</kbd>   | <kbd>zc</kbd> |
| `hs-show-block`    | <kbd>C-c @ C-s</kbd>   | <kbd>zo</kbd> |
| `hs-hide-all`      | <kbd>C-c @ C-M-h</kbd> | <kbd>zm</kbd> |
| `hs-show-all`      | <kbd>C-c @ C-M-s</kbd> | <kbd>zr</kbd> |
| `hs-hide-level`    | <kbd>C-c @ C-l</kbd>   | 无            |
| `hs-toggle-hiding` | <kbd>C-c @ C-c</kbd>   | <kbd>za</kbd> |

一些类似`hideshow`的插件

- [origami](https://github.com/gregsexton/origami.el)
- [folding](https://www.emacswiki.org/emacs/folding.el)
- [yafolding.el](https://github.com/zenozeng/yafolding.el)

其中`origami`有`lsp`支持[lsp-origami](https://github.com/emacs-lsp/lsp-origami)

### hideshow 扩展: 显示被折叠的代码行数

默认情况下`hideshow`对于显示的代码是以`...` `overlay`的形式显示的，而且
`hideshow`给予了自定义的能力，通过设置`hs-set-up-overlay`变量即可。

``` elisp
;; 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
```

附效果图:

![before-fold](https://emacs-china.org/uploads/default/original/2X/c/c204b95093febf0f2455b17e1c98c3c5d7858a13.png)
![after-fold](https://emacs-china.org/uploads/default/original/2X/a/a88653d0d1f48d0e23d1814e46ba998390fc61da.png)

## whitespace

显示空白字符，如`\t` `\f` `\v` 空格等等。

可以配置在`prog-mode`，`markdown-mode`和`conf-mode`下，显示行尾的空白字符。

```elisp
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))
```

当然，仅显示行尾空白字符也可以简单地设置`show-trailing-whitespace`为`t`来开启。

[kinono](https://emacs-china.org/u/kinono) 分享的配置:

```elisp
(use-package whitespace
  :ensure nil
  :hook (after-init . global-whitespace-mode) ;; 注意，这里是全局打开
  :config
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))

  (setq
   whitespace-line-column nil
   whitespace-style
   '(face             ; visualize things below:
     empty            ; empty lines at beginning/end of buffer
     lines-tail       ; lines go beyond `fill-column'
     space-before-tab ; spaces before tab
     trailing         ; trailing blanks
     tabs             ; tabs (show by face)
     tab-mark         ; tabs (show by symbol)
     )))
```

比较好的是能指示过长的行，这样都不需要装那种显示一条竖线的插件了。

![效果图](https://emacs-china.org/uploads/default/optimized/2X/f/f3820ca342843118043eb220ce9cf77d00805f7b_2_690x297.png)

## so-long

有时候会打开一些文件，这些文件里的某一行特别长，而`Emacs`没有针对这种情况做特殊
处理，会导致整个界面卡死。现在它来了！

直接全局启用:

```elisp
(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))
```

当打开一个具有长行的文件时，它会自动检测并将一些可能导致严重性能的`mode`关闭，
如`font-lock` (`syntax highlight`)。

注：`Emacs` 27+ 自带

## glasses

当遇到驼峰式的变量时，如`CamelCasesName`，但是你比较喜欢`GNU`式的命名方式（使用
下划线），那么你可以开启`glasses-mode`。它只会让`CamelCasesName`**显示**成
`Camel_Cases_Name`而不会对原文件做出修改。

不过，大写字母加下划线的组合有点奇怪。

## subword

由[kinono](https://emacs-china.org/u/kinono)分享。

`subword`可以处理`CamelCasesName`这种驼峰式的单词，<kbd>M-f</kbd>
(`forward-word`) 后，光标会依次停在大写的词上。

```elisp
(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))
```

如果不想全局打开，也可以只利用`subword-forward`等移动命令。

此外，`subword`包还提供了一个模式叫做`superword-mode`。在这个模式下，
`this_is_a_symbol`被认为是一个单词。 <kbd>M-f</kbd> (`forward-word`) 可以直接跳
过。

## follow-mode

如果你的屏幕很宽，但是实际显示的条目的宽度无法利用这宽屏幕，那么`follow-mode`可
以帮助你。一个典型的使用案例是，再打开一个窗口，然后对当前`buffer`开启
`follow-mode`，这样之后另一个窗口显示的内容会是当前窗口的后续。例如，一个文件有
100行，当前`buffer`只能显示10行，那么另一个窗口将会显示下面10行。如果嫌窗口数还
是太少，可以继续增多。

![follow-mode](https://emacs-china.org/uploads/default/original/2X/b/b6f11e53b620049c4534a92bd7f22e8f08a15483.png)

## delsel

由[Kermit95](https://emacs-china.org/u/Kermit95)分享。

选中文本后，直接输入就可以，省去了删除操作。这在其他文本编辑器里都是标配，建议打开。

```elisp
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))
```

## parenthesis

高亮显示配对的`( )` `[ ]` `{ }` 括号，比较实用，建议打开。

```elisp
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))
```

## simple

`simple`包提供的基础命令非常多，这里会慢慢完善。

### 在`modeline`里显示行号、列号以及当前文件的总字符数。

```elisp
(use-package simple
  :ensure nil
  :hook (after-init . (lambda ()
                         (line-number-mode)
                         (column-number-mode)
                         (size-indication-mode))))
```

### `shell-command-on-region`的妙用

在注释中看到不认识的单词，一般做法是复制、打开终端、然后调用外部程序
[`ydcv`](https://github.com/farseerfc/ydcv-rs)来翻译。

最近发现了`shell-command-on-region`之后，可以省掉前2个步骤了！

例如`papaya`单词不认识，那么选中它，然后<kbd>M-|</kbd>
(`shell-command-on-region`)运行一下，输入`ydcv`然后执行。如果输出内容比较少，则
直接会在`echo area`处显示（当`resize-mini-windows`不为`nil`时由
`max-mini-window-height`决定）。如果想强制它输出在`buffer`下（方便复制），可以在
上面再封装一层、利用`advice`机制，或者提前创建一个名叫`*Shell Command Output*`的
`buffer`。

以下是一个`advice`的例子（不推荐使用`advice`，有较大的侵入性，这里只是做个演示）

``` elisp
(define-advice shell-command-on-region (:around (func &rest args))
  (let ((max-mini-window-height 0))
    (apply func args)))
```

甚至可以直接封装一个叫做`ydcv`的命令来完成这个工作！

``` elisp
(defun ydcv (beg end)
  (interactive "r")
  (let ((max-mini-window-height 0))
    (shell-command-on-region beg end "ydcv")))
```

当然还可以有`ydcv-at-point`, `ydcv-dwim`等一系列函数！

## autorevert

有时候`Emacs`里打开的文件可能被外部修改，启用`autorevert`的话可以自动更新对应的
`buffer`.

```elisp
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))
```

## isearch

本身`Emacs`自带的`isearch`已经足够强大，稍加修改就可以增加实用性。

例如[`anzu`](https://github.com/emacsorphanage/anzu)的显示匹配个数的功能就已经原
生支持了。通过

```elisp
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ")
```

来显示如 `10/100` 这种状态。

比较恼人的一点是，在搜索中删除字符会回退搜索结果，而不是停在当前位置将最后一个搜
索字符删除。这里可以通过`remap isearch-delete-char`来实现。

此外，还可以将搜索结果保持在高亮状态以方便肉眼识别。这个是通过设置
`lazy-highlight-cleanup`为`nil`实现的。去除高亮状态需要人工`M-x`调用
`lazy-highlight-cleanup`。

```elisp
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))
```

注：`isearch-lazy-count`和`lazy-count-prefix-format`需要`Emacs` 27+

### 令 isearch 像在浏览器里搜索一样

在浏览器里，我们只需要按<kbd>C-f</kbd>，然后敲入所要搜索的字符串。之后只要按回车
就可以不断地向下搜索。如果我们需要向上搜索，那么需要点击一下向上的箭头。

现在我们在`isearch`里模拟这种情况，还是使用<kbd>C-s</kbd>来调用`isearch`。但是之
后的`repeat`操作是交给了回车。

首先，我们先定义一下变量来保存当前搜索的方向。

```elisp
(defvar my/isearch--direction nil)
```

然后使得`isearch-mode-map`下的<kbd>C-s</kbd>可以告诉我们当前是在向下搜索；同理，
使得`isearch-mode-map`下的<kbd>C-r</kbd>告诉我们是在向上搜索。

```elisp
(define-advice isearch-repeat-forward (:after (_))
  (setq-local my/isearch--direction 'forward))
(define-advice isearch-repeat-backward (:after (_))
  (setq-local my/isearch--direction 'backward))
```

这里偷懒，采用了`advise`的方式。如果不想侵入，可以自己在上层包装一下对应的命令。

然后在`isearch-mode-map`下的回车操作就是根据`my/isearch--direction`来搜索了。就
是如此简单。

```elisp
(defun my/isearch-repeat (&optional arg)
  (interactive "P")
  (isearch-repeat my/isearch--direction arg))
```

当然在按`Esc`键的时候表明搜索已经结束了，此时应该重置当前的方式:

```elisp
(define-advice isearch-exit (:after nil)
  (setq-local my/isearch--direction nil))
```

完整代码见下方:

```elisp
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
         ([return] . my/isearch-repeat)
         ([escape] . isearch-exit))
  :config
  (defvar my/isearch--direction nil)
  (define-advice isearch-exit (:after nil)
    (setq-local my/isearch--direction nil))
  (define-advice isearch-repeat-forward (:after (_))
    (setq-local my/isearch--direction 'forward))
  (define-advice isearch-repeat-backward (:after (_))
    (setq-local my/isearch--direction 'backward))
  )
```

## tempo

[`tempo`](https://www.emacswiki.org/emacs/TempoMode)可以算是`yasnippet`的祖先，
`skeleton`算是它的爷爷。由于`tempo`里可以使用`elisp`函数，灵活性非常大。

实际上在写代码的时候，想插入一个**LICENSE**头是个比较常用的需求，它也可以通过其
他方式如`auto-insert`在打开文件时就自动插入。在这里，我们使用`tempo`来实现。

目前比较推荐的方式是采用`SPDX`的格式，而不是直接把`license`内容写入代码文件中。
采用`SPDX`格式可以有效的减少文件大小，不会喧宾夺主占用大量代码行数。

一个典型的`license`头是这样:

``` cpp
// Copyright 2017 - 2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0
```

所以我们可以仿照着这个格式来写一个`tempo`的`template`.

``` elisp
;; 完整的列表非常长，可以访问 https://spdx.org/licenses/ 获得
(defconst license-spdx-identifiers
  '(Apache-1.0 Apache-2.0 MIT))

(tempo-define-template "license"
  '(comment-start
    (format "Copyright %s - present %s Authors"
            (format-time-string "%Y")
            (if (featurep 'projectile)
                (progn
                  (require 'projectile)
                  (projectile-project-name))
              "Unknown"))
    comment-end > n>
    comment-start
    "SPDX-License-Identifier: " (completing-read "License: "
                                                 license-spdx-identifiers)
    comment-end > n>)
  'license
  "Insert a SPDX license.")
```

`tempo`内的`>`表示的是缩进，`n`表示的是插入一个换行，其他的部分就是一个普通的
`elisp`函数了。

这样定义了这个`template`之类，会生成一个叫`tempo-template-license`的函数。因此我
们可以直接调用它来插入`license`头部。

此外还可以结合`abbrev-mode`来自动替换，如果想在`elisp-mode`下直接替代，可以通过
`define-abbrev`来实现：

```elisp
(define-abbrev emacs-lisp-mode-abbrev-table "2license" "" 'tempo-template-license)
```

这里只需要在`elisp-mode`下开启`abbrev-mode`，然后输入`2license `就会实现自动替换
（注意，最后要有一个空格）。

此外，`abbrev-expand`是以 WORD 为展开，如果在`emacs-lisp-mode`下使用`;license`则
会不生效，因为`;license`是由 2 个单词组成的。

PS: 可以通过<kbd>C-h s</kbd> (`describe-syntax`) 来查看当前当前 mode 的 syntax-table.

## align

听说有些写`java`的朋友特别喜欢将变量的`=`对齐，即原来的代码是这样的:

```java
private int magicNumber = 0xdeadbeef;
private double PI = 3.14159265358939723846264;
```

选中它们，然后调用`align-regexp`，给定`=`作为它的参数，就会将上述代码的`=`部分对
齐了。

```java
private int magicNumber = 0xdeadbeef;
private double PI       = 3.14159265358939723846264;
```

其他`align`相关的函数功能还有待开发。

## dired

`dired`是一个用于`directory`浏览的`mode`，功能非常丰富。因此这里介绍的东西肯定不
能完全覆盖，会慢慢完善之。

### 在 dired 中用外部程序打开对应文件

在`dired-mode-map`中，也是可以执行`shell`命令的。与之相关的命令有

- `dired-do-shell-command`, 默认绑定在<kbd>!</kbd>
- `dired-smart-shell-command`，默认绑定在<kbd>M-!</kbd>
- `async-shell-command`，默认绑定在<kbd>M-&</kbd>

其中，通过配置`dired-guess-shell-alist-user`可以令`dired-do-shell-command`有一个
比较好的默认命令。例如，我这是样配置的:

``` elisp
(setq dired-guess-shell-alist-user `((,(rx "."
                                           (or
                                            ;; Videos
                                            "mp4" "avi" "mkv" "flv" "ogv" "mov"
                                            ;; Music
                                            "wav" "mp3" "flac"
                                            ;; Images
                                            "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                            ;; Docs
                                            "pdf" "md" "djvu" "ps" "eps")
                                           string-end)
                                      ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                                             ((eq system-type 'darwin) "open")
                                             ((eq system-type 'windows-nt) "start")
                                             (t "")))))
```

这里考虑了多个平台下的差异。如`linux`平台下会使用`xdg-open`来打开对应的这些文件
(通过`mimeinfo`来配置，见`~/.config/mimeapps.list`)。但是它有一个缺点，会阻塞当
前的`Emacs`进程，所以仅适用于临时查看的需求。

`dired-smart-shell-command`与`dired-do-shell-command`类似，也会阻塞当前`Emacs`进
程。

`async-shell-command`则不会阻塞当前`Emacs`，唯一的缺点可能是会多弹出个`buffer`吧。
如果对`async-shell-command`的结果不是很感兴趣，可以通过`shackle`等类似的工具忽略
对应的`buffer`。

如果使用的是`Emacs` **28**的话，并且已经设置了

``` elisp
(setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
```

可以直接在`dired`里按`W` (`browse-url-of-dired-file`), 这会直接用外部程序打开。
当然，它不会阻塞`Emacs`。

### 隐藏、显示 以`.`开头的文件

`dired`显示文件时使用的`ls`命令参数是由`dired-listing-switches`来控制的，它的默
认值是`-al`。如果不想要显示以`.`开头的文件，那么通过<kbd>C-u s</kbd> (`s`为
`dired-sort-toggle-or-edit`)来重新设置`dired-listing-switches`。

如果只是想简单地隐藏当前目录下以`.`开头的文件，那么可以通过将满足`^\\.`正则的行
删除就行（真实文件并没有删除，只是删除它的显示）。注意到`dired-do-print`命令基本
不怎么使用，于是可以利用`advice`来覆盖它，实现我们自己的`dotfiles-toggle`。

``` elisp
;; 修改自 https://www.emacswiki.org/emacs/DiredOmitMode
(define-advice dired-do-print (:override (&optional _))
    "Show/hide dotfiles."
    (interactive)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (dired-mark-files-regexp "^\\.")
          (dired-do-kill-lines))
      (revert-buffer)
      (setq-local dired-dotfiles-show-p t)))
```

这样只要按一下<kbd>P</kbd>就可以达到隐藏、显示的切换了。

如果不想自己写`elisp`，这里也有一个现成的包 https://github.com/mattiasb/dired-hide-dotfiles

## ispell

`ispell`全称是`interactive` `spell`检查器，它支持`ispell`, `aspell`和`hunspell`，
以下以`hunspell`为例。

``` elisp
;; 这里使用的是 en_US 字典，需要使用包管理安装对应的字典，类似的名字可能 hunspell-en_US
(setq ispell-dictionary "en_US"
      ispell-program-name "hunspell"
      ispell-personal-dictionary (expand-file-name "hunspell_dict.txt" user-emacs-directory))
```

这样就可以通过调用`ispell-word`来看一个单词是否正确了。如果是`evil`用户，这个函
数已经被绑定至<kbd>z=</kbd>上了。 \w/

## calendar

`Emacs`里日历可以拿来干什么呢？

第一个作用自然是看日期的，最起码得让今天醒目得吧？于是选择了在
`calendar-today-visible-hook`上加上`calendar-mark-today`。默认今天的日期是有下划
线的，如果不喜欢也可以自己修改`calendar-today-marker`。

第二个作用自然是看节日的，为了更加更本地化一点，可以设置一些自己想关注的节日。我
是这样设置的:

把较本土的节日放在了`holiday-local-holidays`里,

``` elisp
;; 分别是妇女节、植树节、劳动节、青年节、儿童节、教师节、国庆节、程序员节、双11
(setq holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                               (holiday-fixed 3 12 "Arbor Day")
                               ,@(cl-loop for i from 1 to 3
                                          collect `(holiday-fixed 5 ,i "International Workers' Day"))
                               (holiday-fixed 5 4  "Chinese Youth Day")
                               (holiday-fixed 6 1  "Children's Day")
                               (holiday-fixed 9 10 "Teachers' Day")
                               ,@(cl-loop for i from 1 to 7
                                          collect `(holiday-fixed 10 ,i "National Day"))
                               (holiday-fixed 10 24 "Programmers' Day")
                               (holiday-fixed 11 11 "Singles' Day")))
```

再把其他没在默认日历里的放进`holiday-other-holidays`里,

``` elisp
;; 分别是世界地球日、世界读书日、俄罗斯的那个程序员节
(setq holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
                               (holiday-fixed 4 23 "World Book Day")
                               (holiday-sexp '(if (or (zerop (% year 400))
                                                      (and (% year 100) (zerop (% year 4))))
                                                  (list 9 12 year)
                                                (list 9 13 year))
                                             "World Programmers' Day")))
```

然后再开启`calendar`内置的中国节日支持:

``` elisp
(setq calendar-chinese-all-holidays-flag t)
```

这样就可以获得一个不错的日历体验了。如果自己还有农历节日需求的话，可以使用
`holiday-chinese`来定义。如

``` elisp
;; 元宵节
(setq holiday-oriental-holidays '((holiday-chinese 1 15 "Lantern Festival")))
```

当然元宵节已经默认被定义了，只需开启`calendar-chinese-all-holidays-flag`。

如果这还不够，还有[cal-china-x](https://github.com/xwl/cal-china-x)。

第三个功能也可以在`calendar`界面添加日记，默认的日记从功能上来说自然是不如
`org-mode`加持的丰富。请确保`org-agenda-diary-file`的值不是`'diary-file`，然后在
`calendar-mode-map`下调用`org-agenda-diary-entry`即可选择插入日记。

附图:

![org-agenda-add-entry-to-org-agenda-diary-file](https://emacs-china.org/uploads/default/original/2X/a/a2dbd0a45689694308f31218030d6f95cecb7d93.png)

![org-agenda-diary-file](https://emacs-china.org/uploads/default/original/2X/d/d8fd2964e8999f56e88c55b07043c804d02be37d.png)

需要注意，它默认不会自动保存`org-agenda-diary-file`。如果不喜欢这一点，可以利用
`advice`来修正一下。

``` elisp
(defun org-agenda-add-entry-with-save (_type text &optional _d1 _d2)
  ;; `org-agenda-add-entry-to-org-agenda-diary-file'里认为如果用户没有输入有效的
  ;; 内容，会弹出对应 buffer 让用户人工输入。
  (when (string-match "\\S-" text)
    (with-current-buffer (find-file-noselect org-agenda-diary-file)
      (save-buffer))))

(advice-add #'org-agenda-add-entry-to-org-agenda-diary-file :after #'org-agenda-add-entry-with-save)
```

我觉得这样子设置之后，可以轻度取代
[org-journal](https://github.com/bastibe/org-journal)了?

## strokes

如果你想用鼠标来控制`Emacs`的行为，有点像现在浏览器上的鼠标手势。不过它只能识别
鼠标移动轨迹所描绘的形状，不能判断它的方向。

1. 执行`strokes-mode`打开`minor-mode`
2. 执行`strokes-global-set-stroke`在弹出的`buffer`内使用`Shift`+鼠标左键（也可以
   用中键）绘出想作为快捷操作的大致形状，假设是一个 C 的形状，然后鼠标右键结束绘
   制。稍后会提示输入与`stroke`对应的命令，假设是`strokes-help`
3. 移动鼠标，使得它的轨迹是个 C 的形状
4. `Shift`+鼠标中键以执行与这个`stroke`对应的命令，也就是`strokes-help`

想要更详细的信息？请`M-x strokes-help`.

## webjump

你想在`Emacs`里快速调用搜索引擎搜索吗？原来这个功能早已经内置了！

由于`webjump-sites`早已有默认值了，如果想急着体验一下可以立即`M-x webjump`。其原
理也是相当简单，通过用户选择它想要用的搜索引擎+查询内容构造出实际`url`，然后通过
`browse-url`调用托管给浏览器。

我的配置是这样的:

``` elisp
(use-package webjump
  :ensure nil
  :bind ("C-c /" . webjump)
  :custom
  (webjump-sites '(
                   ;; Emacs.
                   ("Emacs Home Page" .
                    "www.gnu.org/software/emacs/emacs.html")
                   ("Savannah Emacs page" .
                    "savannah.gnu.org/projects/emacs")

                   ;; Internet search engines.
                   ("DuckDuckGo" .
                    [simple-query "duckduckgo.com"
                                  "duckduckgo.com/?q=" ""])
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Google Groups" .
                    [simple-query "groups.google.com"
                                  "groups.google.com/groups?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))
```

如果只是想要简单的查询，那么可以作为
[engine-mode](https://github.com/hrs/engine-mode)的内置替换方案了。

当然，还可以配置多级查询选项，可参考`webjump-to-iwin`的实现。

## transient-map 小技巧

因为`transient-map`的优先级比其他`keymap`都要高，所以可以将它当作菜单来使用。

如果嫌`set-transient-map`用起来不方便，可以使用[hydra](https://github.com/abo-abo/hydra)代替。

### 编辑时拷贝 (配合 avy)

如果在编辑文字时发现要拷贝一个`url`，但是当前窗口内有多个`url`，类似的场景如下:

``` C++
/// url1: https://www.google.com
/// url2: https://www.baidu.com
/// url3: https://duckduckgo.com
int foo(int x) {
    const char* url = "";
}
```

想要为`url`赋值为注释内的 3 个 url 之一。

1. 首先将光标移动到想复制的`url`处
2. 再将这个`url`复制到`kill-ring`当中
3. 再回到原来的位置
4. 再粘贴

``` elisp
(defhydra hydra-copy (:color blue)
  "Copy"
  ("w" copy-word-at-point "word")
  ("u" copy-url-at-point "url")
  ("q" nil "cancel"))

(defun copy-url-at-point ()
  "Copy url at point."
  (interactive)
  (save-excursion
    (avy-goto-word-or-subword-1)
    (kill-new (thing-at-point 'url))))
```

这里使用`hydra`来偷懒一下。

效果图:

![copy-url-at-point](https://emacs-china.org/uploads/default/original/2X/c/c7c4c60760fb52fe87d095f7ab7828917b13cd64.gif)

## type-break

历史老物，1994 年的时候就已经出现了。

打字打累了，想休息一下？看代码看累了，想放松一下？

那么它可能会适合你。如果在一段时间内的敲击键盘次数大于阈值，那么它会假设平均速度`35 wpm`，每个单词长度5来推算出要休息多少分钟。

而到达休息状态时，它可能会显示出一个汉诺塔移动的动画。可以`M-x type-break`立即体验！

## timeclock

这是一个计算时间到底去哪里了的包，不过都有`org-mode`了，真的还会有人来用这个吗？

| org             | timeclock       |
|-----------------|-----------------|
| `org-clock-in`  | `timeclock-in`  |
| `org-clock-out` | `timeclock-out` |

功能与`org-mode`几乎一致，不过它可以随时`timeclock-out`不用管记录时间的文件打开与否，而在`org-mode`中`clock-out`则要保证运行`clock`的那个文件还处于打开状态。

## elide-head

依旧是怀旧向的内置包，可以将源代码文件的头部中大量的`license`说明折叠起来，效果
跟`hideshow`包类似。可以通过配置`elide-head-headers-to-hide`来自定义想要的折叠区间。

## midnight 深夜模式

在晚上零点的时候定期执行一些任务，默认是`clean-buffer-list`，可以设置`midnight-hook`来自定义行为。

<kbd>M-x midnight-mode</kbd> 来开启深夜模式。嗯，又到了深夜网抑云音乐时间了。

## term mode 相关应用

Emacs 下有几个类似终端模拟器（其实有些不算是），内置的有这 3 个: `shell-mode`, `term-mode`, `eshell`。

如果你不喜欢用 <kbd>M-x compile</kbd> 来编译，习惯在 `shell-mode`, `term-mode`, `eshell` 下直接使用 gcc 或者 make 来编译, 那么你可能需要`compilation-shell-minor-mode`。它可以识别报错，令错误可以点击，快速打开报错文件。自然在调用 <kbd>M-x compile</kbd> 的就是用的 `compilation-mode` 了。

### term-mode

`term-mode` 算是一个完整的终端模拟器，与外部的终端模拟器相比除了刷新速度慢、色彩显示较差之外就没有其他差别了。因此如果是在 Linux/MacOS 平台下且只在本地使用，是比较推荐 `term-mode` 的。`term-mode` 分别可以通过 `term` 和 `ansi-term` 命令启动，唯一的区别是由 `term` 命令启用的终端模拟器下面 <kbd>C-x</kbd> 是直接被终端给捕获了，想要在这个模式下使用 <kbd>C-x C-f</kbd> 来打开文件还需要再额外地做设置，而且重复地使用 `term` 命令只会打开一个 buffer。如果你想多次调用分别打开多个 buffer 的话推荐使用 `ansi-term` 命令。对我个人而言我是更喜欢 `ansi-term` 命令。

``` elisp
;; 可以使用这种方式将需要的按键解绑
(use-package term
  :ensure nil
  :bind (:map term-raw-map
         ("M-:" . nil)
         ("M-x" . nil)))
```

当然在 `term-mode` 里使用 `htop`, `git`, `fzf`, `neofetch` 这种类似工具是没啥大问题的，但是使用 `vim` 的话就有点拉胯了。一是显示效果非常差，代码高亮都无法显示；二是也不推荐在 Emacs 里使用 `vim`, 编辑文件直接 <kbd>C-x C-f</kbd> 就好。

![htop in term-mode](https://emacs-china.org/uploads/default/original/2X/f/f053ce7513b6e319af1f6eb66403b5cd8ed8110e.png)

![term-mode vs alacritty](https://emacs-china.org/uploads/default/original/2X/4/4eb784393e618176322fca98f4f8556debd5c707.png)

这里不得不提一下, `term-mode` 里两种模式，一个是 `char-mode`, 另一个是 `line-mode`​。 在 `char-mode` 下输入任意一个字符都会直接转发至当前的进程，而 `line-mode` 下则只会遇到 `\n` 的时候才会将以前的内容一起转发。就拿 `htop` 这个命令来说，在 `char-mode` 下按一下 `q` 会直接退出，按一下 `C-n` 会移动光标，但是一旦切换到 `line-mode` 下后就完全变了，连续地按 `q` 不会退出，直到你按下 Enter 键。

`term-mode` 还有一个非常大的优点是与 Emacs 生态的结合。其中一个是可以快速地跳转到上一次的 prompt 处（一般的终端模拟器都没有这个功能），想要启用这功能需要配置 `term-prompt-regexp` 变量，而它的默认值非常不友好，竟然是一个 `^` 表示跳转到开头。建议修改成如下配置，毕竟我们要在 `term-mode` 里使用 shell, 这个配置也是它的注释里所推荐使用的:

``` elisp
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
```

这样就可以使用 <kbd>C-c C-p</kbd> 和 <kbd>C-c C-n</kbd> 来上下跳转 prompt 了。

另外一点是目录同步，如果你在 `term-mode` 下进入了 `/tmp` 目录，那么在 Emacs 按 <kbd>C-x C-f</kbd> 就会尝试打开此目录下的文件。如果你是 bash 用户那么这个甚至不需要你配置，其他 shell 用户就必须要在对应 shell 的配置里增加如下配置:

``` shell
# 这是 zsh 需要做的修改
#
# INSIDE_EMACS 则是 Emacs 在创建 term/shell/eshell 时都会带上的环境变量
# 通常 shell/tramp 会将 TERM 环境变量设置成 dumb，所以这里要将他们排除。
#
# shell 下的目录同步不采用这种方式
function precmd() {
  if [[ -n "$INSIDE_EMACS" && "$TERM" != "dumb" ]]; then
    echo -e "\033AnSiTc" "$(pwd)"
    echo -e "\033AnSiTh" $(hostname -f)
    echo -e "\033AnSiTu" "$LOGNAME"
  fi
}
```

更详细的说明可以见 [AnsiTermHints](https://www.emacswiki.org/emacs/AnsiTermHints)。

其实它就是在每条命令执行前将自己当前的目录告诉了 `term-mode`, 然后 `term-mode` 再设置 `default-directory` 变量。

![directory track in term-mode](https://emacs-china.org/uploads/default/original/2X/8/894a39c9b662d01199f141d561e8898859404791.png)

另外一种方式则是依赖 Linux 的 `procfs`, 可以获得 `term-mode` 启动的 shell 进程 pid，然后通过读 `/proc/pid/cwd/` 来获取当前路径。

``` elisp
(defun term-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when term-process
    (let* ((pid (process-id term-process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))

;; term-process 则是在 term-mode-hook 中通过
;;
;; (get-buffer-process (current-buffer))
;;
;; 获得

;; 注：以上这种方式对于 vterm 同样适用，因为 vterm 直接暴露了 vterm--process 使用
;; 起用更加方便
```

如果你嫌 `term-mode` 的刷新速度太慢、颜色显示太差，可以使用 `vterm`, 但是它的目录同步方式完全与 `term-mode` 不同，这点需要注意。

### hacking term-mode

Emacs 自带一个 `term-paste` 函数，可以在 char mode 里粘贴文本。不过有时候我们复制的文本最后有个换行，一粘贴就会立即运行，很讨厌。下面这个版本处理了有换行符和多行的情况：

``` elisp
(defun my-term-yank ()
  "Paste recent kill into terminal, in char mode."
  (interactive)
  (when-let ((text (current-kill 0))
             ;; Remove newlines at the beginning/end.))
             (text (string-trim text "\n+" "\n+")))
    (when (or (not (string-match-p "\n" text))
              (y-or-n-p "You are pasting a multiline string.  Continue? "))
      (term-send-raw-string text))))
```

可以把它绑定到 `term-raw-map` 的 `C-M-v` 上，用起来就和专门的终端模拟器差不多了。

下面的代码提供了 `my-term-browse-mode` 命令，执行以后就会把终端变成一个只读的普通 buffer，快捷键也和普通 buffer 一样，方便我们浏览比较长的输出或者复制东西。

``` elisp
(defvar my-term-browse-mode-map
  (make-sparse-keymap)
  "Keymap for `my-term-browse-mode'.")

(defun my-term-browse-mode ()
  "Turn the terminal buffer into a read-only normal buffer."
  (interactive)
  ;; Workaround: Without this code, there's a bug: Press `C-p' in char mode to
  ;; browse history, then `C-n' to go back, then `my-term-browse-mode', then
  ;; `C-n', you'll find a newline is produced.  Call `term-char-mode', that
  ;; newline is sent to the shell.  This is not a problem with
  ;; `my-term-browse-mode', since `term-line-mode' also has it.
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (while (eq (char-before) ?\n)
        (delete-char -1))))
  ;; Idea: We could put a `read-only' property to the region before
  ;; `process-mark', so current input could be edited, but I think there's
  ;; little benefit.
  (setq buffer-read-only t)
  (remove-hook 'pre-command-hook #'term-set-goto-process-mark t)
  (remove-hook 'post-command-hook #'term-goto-process-mark-maybe t)
  (use-local-map my-term-browse-mode-map))
```

browse mode 下的键位由 `my-term-browse-mode-map` 指定，可以把 `term-char-mode`, `term-previous-prompt`, `term-next-prompt` 等命令绑定在里面。

## shell-mode

`shell-mode` 它实际上不算是一个终端模拟器，它只是简单包装了一下 shell, 所以只能执行一些简单的命令， `htop` 这种存在复杂交互的应用就不行了。它也支持上下跳转到 prompt 处，而且它的默认值足够通用，如果不适用的话用户再自己配置一下 `shell-prompt-pattern`. 通过 <kbd>C-c C-p</kbd> 和 <kbd>C-c C-n</kbd> 来上下跳转 prompt.

自然 `shell-mode` 也支持目录同步，不过它的同步方式与 `term-mode` 不同。 `term-mode` 是要求 shell 主动告诉 Emacs，而 `shell-mode` 是启用了 `shell-dirtrack-mode` 使用正则匹配如 `cd`, `pushd` 等各种可能改变当前目录的各种命令来达到的。

与 `term-mode` 相比而言它实在是没啥多大优势，但是如果你是在通过 `tramp` 编辑一个远程的文件，想在远程机器上运行一些命令，可以直接 <kbd>M-x shell</kbd> 登录远端的机器，而 `term-mode` 则不会识别这种情况，仍是创建一个本地的终端环境。在有 `tramp` 的情况下, `shell-mode` 下路径显示在 `cd` 改变了当前工作目录之后会显示出错， PR 的机会又来了！

![shell-mode vs term-mode](https://emacs-china.org/uploads/default/original/2X/0/048bf1e000bbe91b504b7b142745a25743ab631c.png)

在 `shell-mode` 里没法像终端模拟器那样通过 <kbd>M-.</kbd> 来直接输入上一命令的最后一个参数，但是多数 shell 都实现提供了一个内部变量 `$_` 支持。

``` shell
echo hello
echo $_

# 输出如下
#
# hello
# hello
```

另外一个独到的地方是它可以当做 sh 文件的 REPL。例如你在编写这样的一个 sh 脚本:

``` bash
echo 'hello, world'
```

可以直接输入 <kbd>C-c C-n</kbd> (`sh-send-line-or-region-and-step`) 将当前行发送至 shell 执行。

![shell repl](https://emacs-china.org/uploads/default/original/2X/7/7bec91f50446aeaf57aab5fcff8247c39442a128.png)

### eshell

`eshell` 则是完全由 elisp 实现的 shell，正因为它是 elisp 实现的，所以在所有地方下都可以使用（推荐 Windows 用户使用）。当然也因为它是 elisp 实现的，所以速度上会稍微慢一点。此外如果你是在远程编辑文件，那么使用 `eshell` 可以直接编辑远程文件，因为它是完全用 elisp 实现的，可以共享当前 Emacs 的状态，自然 tramp 也是可以直接共享的，当然目录同步的更不在话下了。

它的语法与与 bash/zsh 的语法不完全一致，例如一个 for 循环

``` shell
# 在 bash/zsh 里这样写的
for i in *.el; do
  rm $i
done

# 在 eshell 里则是这样写的
for i in *.el {
  rm $i
}
```

在 `eshell` 里执行类似 `htop`, `git diff` 这样的命令也是可以的，但是它不是直接支持，而是间接调用 `term-mode` 来完成。只需要将对应的命令加入至 `eshell-visual-commands`, `eshell-visual-subcommands` 和 `eshell-visual-options` 中即可，建议配置:

``` elisp
(use-package em-term
  :ensure nil
  :custom
  (eshell-visual-commands '("top" "htop" "less" "more" "bat"))
  (eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show")))
  (eshell-visual-options '(("git" "--help" "--paginate"))))
```

因为 `eshell` 不能复用其他 shell 的插件，所以 `eshell` 有自己的生态，可以考虑使用 `eshell-git-prompt` 等包。

`eshell` 还有一个最大的缺点是补全系统。其他 shell 都有自己的 `bash-completion`, `zsh-completion` 包，但是 eshell 却没有，但是它只提供了一个基础的补全功能模块 `pcomplete`. 通过它我们也可以完成基础命令的补全，但是如果想要全部实现的话还是得费一番功夫的。基于这个痛点，Emacs 社区有相应的增强包 [emacs-fish-completion](https://github.com/ambrevar/emacs-fish-completion), 在补全时将对应的命令发送给 fish 然后再截获、解析它的输出。以这种形式扩展的 pcomplete 不用再重复走 `bash-completion`, `zsh-completion` 的路。

因为 `eshell` 与 `shell-mode` 都使用了 pcomplete, 所以这两者都能够享受到由此带来的补全效果。

默认情况下，在 `eshell` 里 <kbd>C-d</kbd> 只会删除字符不会在当前输入为空时退出、 <kbd>M-.</kbd> 不会自动插入上一命令的最后一个参数，这可能会令习惯使用外部 shell 的用户非常不习惯，可以通过如下配置将它们带回来。

``` elisp
;; eshell 自己有实现的一个比较好的 C-d 函数，但是它默认没有开启
;; 这里显式地将这个函数导出。
(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

;; Emacs 28 可以直接定义在 eshell-mode-map 里，但是 27 的话需要将相关的键绑定定义在
;; eshell-first-time-mode-hook 这个 hook 里
(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
         ("C-d" . eshell-delchar-or-maybe-eof)
         ("M-." . eshell-yank-last-arg))
  :config
  (defun eshell-yank-last-arg ()
    "Insert the last arg of the previous command."
    (interactive)
    (insert "$_")
    (pcomplete-expand))
  )
```

关于 `$_` 的说明可以看 [eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html) 文档的 Expansion 节。 eshell 这样设计也与其他 shell 保持一致。
