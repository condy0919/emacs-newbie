# Emacs builtin modes 功能介绍

`Emacs`自带的`mode`功能也比较强大，而一般初学者（比如我）使用`Emacs`时间较短，对
它自身强大的`mode`不了解而错失一些可以提高生产力的工具。

## winner-mode

`winner-mode`是一个全局的`minor mode`，它的主要功能是记录窗体的变动。例如当前有
2 个窗口，然后你关了一个，这时可以通过`winner-undo`来恢复。还可以再`winner-redo`
来撤销刚才的`undo`.

它默认按键绑定为:

```
(C-c <Left>) winner-undo
(C-c <Right>) winner-redo
```

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

## saveplace

`saveplace`记录了上次打开文件时光标停留在第几行、第几列。如果不想每次打开文件都
要再次跳转到上次编辑的位置，这个`mode`可以轻松地应对这种情况。

建议配置:

```elisp
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))
```

## hl-line

高亮当前行。

```elisp
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))
```

## hideshow

隐藏、显示结构化数据，如`{ }`里的内容。对于单函数较长的情况比较有用。

建议配置：

```elisp
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
         ("C-c TAB" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
           '((c-mode "{" "}" "/[*/]" nil nil)
             (c++-mode "{" "}" "/[*/]" nil nil)
             (rust-mode "{" "}" "/[*/]" nil nil)))))
```

一些类似`hideshow`的插件

- [origami](https://github.com/gregsexton/origami.el)
- [folding](https://www.emacswiki.org/emacs/folding.el)
- [yafolding.el](https://github.com/zenozeng/yafolding.el)

其中`origami`有`lsp`支持[lsp-origami](https://github.com/emacs-lsp/lsp-origami)

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

## subword

由[kinono](https://emacs-china.org/u/kinono)分享。

`subword`可以处理`CamelCasesName`这种驼峰式的单词，`M-f` (`forward-word`) 后，光
标会依次停在大写的词上。

```elisp
(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))
```

如果不想全局打开，也可以只利用`subword-forward`等移动命令。

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

在`modeline`里显示行号、列号以及当前文件的总字符数。

```elisp
(use-package simple
  :ensure nil
  :hook (after-init . (lambda ()
                         (line-number-mode)
                         (column-number-mode)
                         (size-indication-mode))))
```

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

## tempo

[`tempo`](https://www.emacswiki.org/emacs/TempoMode)可以算是`yasnippet`的祖先，
`skeleton`算是它的爷爷。由于`tempo`里可以使用`elisp`函数，灵活性非常大。

实际上在写代码的时候，想插入一个**LICENSE**头是个比较常用的需求，它也可以通过其
他方式如`auto-insert`在打开文件时就自动插入。在这里，我们使用`tempo`来实现。

目前比较推荐的方式是采用`SPDX`的格式，而不是直接把`license`内容写入代码文件中。
采用`SPDX`格式可以有效的减少文件大小，不会喧宾夺主占用大量代码行数。

一个典型的`license`头是这样:

```cpp
// Copyright 2017 - 2018 ccls Authors
// SPDX-License-Identifier: Apache-2.0
```

所以我们可以仿照着这个格式来写一个`tempo`的`template`.

```elisp
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
(define-abbrev emacs-lisp-mode-abbrev-table ";license" "" 'tempo-template-license)
```

这里只需要在`elisp-mode`下开启`abbrev-mode`，然后输入`;license `就会实现自动替换
（注意，最后要有一个空格）。
