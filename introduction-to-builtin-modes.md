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
