# ivy 新手教程

## ivy 是什么

`ivy`是一个交互式补全工具，可以应用在如`M-x`命令列表，`find-file`打开文件等场景。

当然提到了`ivy`肯定会提到`counsel`、`swiper`。`swiper`功能比较单一，类似于
`isearch`的搜索工具，而`counsel`集成功能较多，如`counsel-M-x`,
`counsel-find-file`等等。可以通过`M-x`搜索`counsel-`前缀获得更加比较完整的列表。

`ivy`功能非常强大，因此推荐全局打开。

```elisp
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode))
```

一旦这样打开之后，`ivy`默认会重新映射这2个全局操作至`ivy`的对应版本：`C-x b`
(`switch-to-buffer`) 和 `C-x 4 b` (`switch-to-buffer-other-window`)。

关于`use-package`的配置说明，请参阅[这](https://github.com/jwiegley/use-package).

接下来的内容可以算是[ivy manual](https://oremacs.com/swiper/)的翻译 + 一些图片辅
助说明。操作都由[这份配置](https://github.com/condy0919/.emacs.d)完成。因为已经
开启了`counsel-mode`，所以`find-file`映射到了`counsel-find-file`, `M-x`映射到了
`counsel-M-x`.

## ivy minibuffer

因为`ivy`大多数时候基本都工作在`minibuffer`上，因此先介绍一下`minibuffer`是什么。

`minibuffer`是`Emacs`命令读取参数的地方，如`C-x C-f` (`find-file`) 会打开一个文件选项列表供选择，`C-x * q` (`calc` quick mode) 可以打开一个计算器，让用户输入算术表达式计算。

![quick calc](https://emacs-china.org/uploads/default/original/2X/7/745229f4121743ffd1e918bd1fc8e2ec435cda93.png)

![quick calc result](https://emacs-china.org/uploads/default/original/2X/c/c9caf7249090c7fefe294075840f27fc9e028a31.png)

## ivy minibuffer 按键绑定

`ivy-minibuffer-map`提供了6个比较基础的移动方式绑定：

- `C-n`下一个选项
- `C-p`上一行选项
- `M-<`第一个选项
- `M->`最后一个选项
- `C-v`向下翻一页，一页内的候选项数目由`ivy-height`确定
- `M-v`向上翻一页，同上

可以看见，这6个移动方式与`Emacs`在编辑文本时的操作是一致的。

同时，`ivy`也提供了2个与历史操作有关的按键。

- `M-p`上一条历史记录
- `M-n`下一条历史记录

除此之外，`C-a` (`move-beginning-of-line`)、`C-e` (`move-end-of-line`) 等操作方式一样在`minibuffer`下同样适用。

`ivy-height`同它名字显示一样，是指`ivy` `minibuffer`展开的高度，默认值为**9** + **1**。**1** 行当前输入，**9**行其他选项。

![ivy-height](https://emacs-china.org/uploads/default/original/2X/1/17e321a2a227dfe25f62ea591c19d61c2c18d7b3.png)

## ivy action

`ivy`在`incremental completion`完成之后，还可以再执行`action`操作，每个可选择的选项都有所谓的`action`操作。例如`find-file`的默认`action`是打开文件，`M-x`的默认`action`是执行命令。

单纯的列出命令并说明比较无趣，因此下面会结合场景来介绍各种`action`的作用。

### ivy-done

`ivy-done`默认绑定在`C-m`和`RET`上。

它是最常用的操作方式，如`find-file`的默认`action`是打开文件，触发`ivy-done`之后就会
执行这个默认`action`打开对应文件。

### ivy-alt-done

`ivy-alt-done`默认绑定在`C-j`上。

在`find-file`的场景下，例如当前光标停在`auto-save-list`选项上

![before-ivy-alt-done](https://emacs-china.org/uploads/default/original/2X/4/4901781dc6e193c5e21e843c28b6ce27940b48bf.png)

然后触发`ivy-alt-done`就会将当前的目录路径补全。

![after-ivy-alt-done](https://emacs-china.org/uploads/default/original/2X/e/e6fa512938b056610d0e5efc99b42509b4c1c34a.png)

如果接下来的补全选项是要打开的文件，那么再触发`ivy-alt-done`实际的效果跟手工按
`RET`没区别，都会执行打开操作。

![ivy-alt-done-open-file](https://emacs-china.org/uploads/default/original/2X/1/1967b263b44eaf6370bbf1b504d5d498e810c567.png)

如果当前补全选项是`.`或者`..`目录，那么再次触发`ivy-alt-done`实际也跟手工按
`RET`没区别，虽然执行的是`dired`。

![ivy-alt-done-open-dir](https://emacs-china.org/uploads/default/original/2X/b/b5c5134e24484d982b371191cf4436362b817c17.png)

这里还有一个比较简便方法。如果当前路径确定是目录，可以通过直接按`/`来补全目录路径。

![ivy-alt-done-alternative](https://emacs-china.org/uploads/default/original/2X/3/300fd4b6a601031c314a78a38f2dfce0982cf524.png)

在上面这种情况下，直接按`/`与`ivy-alt-done`效果一致。

### ivy-partial-or-done

`ivy-partial-or-done`默认绑定在`TAB`上。

它的功能是尽可能的补全当前的输入，以达到最长前缀状态，也就是**当前输入的字符串是剩余选项的最长公共前缀**。

如图所示, 当前最长的公共前缀应该是`evil-for`

![before-ivy-partial-or-done](https://emacs-china.org/uploads/default/original/2X/a/a6f174888c1764e9c4c67ef7cc25f4754d252f24.png)

这里触发`ivy-partial-or-done`之后，可以看到当前输入已补全变成了`evil-for`了

![after-ivy-partial-or-done](https://emacs-china.org/uploads/default/original/2X/2/2f5b28c4d3fc5a04312bcfac97c2189944213b34.png)

`TAB TAB`的效果与`ivy-alt-done`效果一致，不再赘述。

### ivy-immediate-done

`ivy-immediate-done`默认绑定在`C-M-j`上。

它的作用是以当前用户输入作为结果而不是当前`minibuffer`里的候选项，在
`find-file`的时候特别有用。当要打开一个名为`abc`的文件，但是当前目录下已存在
`abc.pdf`文件。这时候`find-file`的候选项只有一个`abc.pdf`。这个时候通过
`ivy-immediate-done`来告诉`ivy`使用当前的输入`abc`作为结果而不是`minibuffer`里的
候选项。

![new-file](https://emacs-china.org/uploads/default/original/2X/a/ad71d2d4d7123cbd309624f79f0bf100a2135eba.png)

如果是为了解决上述的`find-file`问题，也可以通过设置`ivy-use-selectable-prompt`为
`t`来曲线救国。

![another-way](https://emacs-china.org/uploads/default/original/2X/8/84519d5f540488c020f7a3d1e407ea595f7e081c.png)

注意，这里的光标已经上移至用户输入口了。此时直接`RET`确定就可以打开`abc`文件了。

[相关问题](https://emacs-china.org/t/ivy-find-file/12087)。

### ivy-avy

`ivy-avy`默认绑定在`C-'`上。

当`ivy-height`设置得比较大时，通过`avy`来直接跳转选择结果会是一个比较快速的办法。

![ivy-avy](https://emacs-china.org/uploads/default/original/2X/f/f2ad674d5a2b7b607b17a0d53fed0b313cf20f36.png)

### ivy-dispatching-done

`ivy-dispatching-done`默认绑定在`M-o`上。

当触发`ivy-dispatching-done`之后，它会要求用户输入对应的`action`操作。默认提供的`action`非常之多，可以使用`describe-variable`查看`ivy--actions-list`得到完整列表。

下面以`counsel-M-x`的为例子说明:

`counsel-M-x`下提供了 2 个`action`分别是:

- `d` 查看定义
- `h` 帮助

![M-x evil-forward-char](https://emacs-china.org/uploads/default/original/2X/d/d0eaa05fa71b217fe3d654db59286034f80e1620.png)

这里`M-x`想执行`evil-forward-char`这个命令，但是我想知道这个命令是如何实现的。那
么接下来就通过按`M-o`触发`ivy-dispatching-done`，之后再输入`d`执行查看这个函数的
定义。

![ivy-dispatching-done definition](https://emacs-china.org/uploads/default/optimized/2X/1/1a67e0225bdc217f28ed280f014ad6cfbe9fba05_2_453x500.png)

可以看到，它已经确定跳转至`evil-forward-char`这个命令处了。

而`h`所对应的帮助`action`实际就是`describe-function`的效果。

![ivy-dispatching-done help](https://emacs-china.org/uploads/default/optimized/2X/f/f0dd0c5034e01f493e83f38238a74c848468306f_2_690x310.png)

### ivy-call

`ivy-call`默认绑定在`C-M-m`上，它可以看做不退出`minibuffer`的`ivy-done`操作。

还是以`evil-forward-char`举例

![before-ivy-call](https://emacs-china.org/uploads/default/original/2X/2/2cc99391cd9c67ff5bb4b61693c795cc4b1f4cd3.png)

初始光标停留在行首，然后连续3次`C-M-m`触发`ivy-call`可以得到如下结果。

![after-ivy-call](https://emacs-china.org/uploads/default/original/2X/0/0a7c294f8fb5a9605c3bd5338f39a4a5043b2016.png)

这时，光标已经向前移动了3个字符，且`minibuffer`还处于打开的状态，颇有`hydra`的味道。

### ivy-dispatching-call

`ivy-dispatching-call`默认绑定在`C-M-o`上，对应为`ivy-dispatching-done`的不关闭`minibuffer`版本。

`M-x`输入`evil-forward-char`之后，`C-M-o` `d` `C-M-o` `h`可以分别打开函数定义处、
函数帮助页面。

![ivy-dispatching-call](https://emacs-china.org/uploads/default/optimized/2X/d/d00ba008897db831e5fd0f670bd468657af21104_2_454x500.png)

### ivy-next-line-and-call 和 ivy-previous-line-and-call

他们两个分别默认绑定在`C-M-n`和`C-M-p`上，主要功能是把移动和`action`执行合在一起。

一个比较有用的场景是，`find-file`找到了许多相似的文件，需要一次性都把他们打开。这个时候，`ivy-next-line-and-call`的用处就来了。

![before-ivy-next-line-and-call](https://emacs-china.org/uploads/default/original/2X/b/bcc57fb9d67c044a4dc9f21b80e5607c9d51eb3c.png)

这里想一次性打开`ace-window`的这3个文件，连续3次使用`C-M-n`就行。

![after-ivy-next-line-and-call](https://emacs-china.org/uploads/default/original/2X/4/47429938faa040a1190e15b797165d93e8d98e39.png)

### ivy-resume

`ivy-resume`默认没有提供按键绑定。如同它名字一样，提供`resume`功能，继续上一次的`ivy`操作。

如`M-x`执行`evil-forward-char`，但是不小心按快了，执行了`evil-forward-word-end`. 这时，`ivy-resume`会恢复`M-x`的最后状态，保留着用户输入的内容、光标位置。

![ivy-resume](https://emacs-china.org/uploads/default/original/2X/3/38686f6645e1e906dad62584dbe32028f61076c7.png)

### ivy-next-history-element 和 ivy-previous-history-element

他们两个分别默认绑定在`M-n`和`M-p`上，可方便地查看历史命令。

### ivy-insert-current

`ivy-insert-current`默认绑定在`M-i`上。

不会用，待我再研究一下…

### ivy-yank-word

`ivy-yank-word`默认绑定在`M-j`上。它的功能跟`C-s C-w` (`isearch-mode-map`下的`isearch-yank-word-or-char`) 类似。

例如，当前光标后面有`apple boy cat`这3个单词。打开`M-x`，然后`M-j`触发
`ivy-yank-word`会将第1个单词`apple`输入至`minibuffer`中。然后光标会移动至`boy`单
词前。再次`M-j`即会将`boy`单词输入至`minibuffer`中。

![ivy-yank-word](https://emacs-china.org/uploads/default/original/2X/1/18758fb9b34b1eeca2d6057025bd674143d8b84d.png)

### ivy-restrict-to-matches

`ivy-restrict-to-matches`默认绑定在`S-SPC`上，可以当成一种二次过滤的手段来使用。
它会清空用户输入、将搜索集合设置为当前的候选集合，这样用户可以慢慢地减少干扰项，
缩小查找范围。

![before-ivy-restrict-to-matches](https://emacs-china.org/uploads/default/original/2X/5/5f79a297aa9b32e7a0978d4fa91dbd24413b1495.png)

默认`M-x`可执行的命令数量为 **6247** 。输入`evil`再`S-SPC`触发
`ivy-restrict-to-matches`。此时，当前集合大小仅为 **378** 。可见确实起到了二次过滤
的作用。

![after-ivy-restrict-to-matches](https://emacs-china.org/uploads/default/original/2X/8/8e5ed7e19aa3a084df94d6856f44da4990044323.png)

注：显示候选集合大小通过设置`(setq ivy-count-format "%d/%d")`实现。

[相关问题](https://emacs-china.org/t/ivy-occur/12083)

### ivy-reverse-i-search

`ivy-reverse-i-search`默认绑定在`C-r`上，其功能类似`bash`的反向搜索历史。

### ivy-kill-ring-save

`ivy-kill-ring-save`默认绑定在`M-w`上，它会复制当前搜索集合内所有元素。如果集合
过多，可以配合上述的`ivy-restrict-to-matches`减小集合大小。

### hydra-ivy/body

`hydra-ivy/body`默认绑定在`C-o`上，会弹出一个`ivy`的`hydra`菜单供用户调用。

`hydra`使用说明见[这个](https://github.com/abo-abo/hydra)

| Short | Normal    | Command name              |
|-------|-----------|---------------------------|
| `o`   | `C-g`     | `keyboard-escape-quit`    |
| `j`   | `C-n`     | `ivy-next-line`           |
| `k`   | `C-p`     | `ivy-previous-line`       |
| `h`   | `M-<`     | `ivy-beginning-of-buffer` |
| `l`   | `M->`     | `ivy-end-of-buffer`       |
| `d`   | `C-m`     | `ivy-done`                |
| `f`   | `C-j`     | `ivy-alt-done`            |
| `g`   | `C-M-m`   | `ivy-call`                |
| `u`   | `C-c C-o` | `ivy-occur`               |

由于我本人也不使用`hydra`，就不截图了。

### ivy-occur

`ivy-occur`默认绑定在`C-c C-o`上，它可以将当前的候选集合保存至`buffer`内并退出
`minibuffer`.

`ivy-occur-mode`下所提供的按键绑定(完整列表的可以`describe-variable`查看
`ivy-occur-mode-map`得到)为:

| key          | command                 |
|--------------|-------------------------|
| `RET` or `f` | `ivy-occur-press`       |
| 鼠标左键点击 | `ivy-occur-click`       |
| `j`          | `next-line`             |
| `k`          | `previous-line`         |
| `a`          | `ivy-occur-read-action` |
| `o`          | `ivy-occur-dispatch`    |
| `q`          | `quit-window`           |

主要是使用 `j` `k` 来向下、向上移动，`q` 来退出。

`ivy-occur`扩展性很强，下文会单独开一节描述。

注: 如果是`evil`用户，可能会需要手工将 **normal** 状态变为 **insert** 状态才能使用对应的按键绑定。

```elisp
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :definines (evil-insert-status-cursor)
  :hook ((after-init . ivy-mode)
         (ivy-occur-mode . (lambda ()
                             (setq-local evil-insert-status-cursor 'box)
                             (evil-insert-state)))))
```

#### ivy-occur-dispatch

在`ivy-occur-mode`下，`ivy-occur-dispatch`如同`ivy-dispatching-done`一样，将会读
取一个`action`，然后在当前光标停留的候选项上执行对应的`action`。

![ivy-occur](https://emacs-china.org/uploads/default/optimized/2X/8/802c07403a45544da4b6b3038f1cd555b21a7ac5_2_275x500.png)

## ivy completion style

`ivy`总共提供了5种补全方式：

- `ivy--regex`
- `ivy--regex-plus`
- `ivy--regex-ignore-order`
- `ivy--regex-fuzzy`
- `regexp-quote`

默认配置为

```elisp
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
```

`t`是用来保证每个操作都有一个对应的`re-builder`可以使用。

### ivy--regex-plus

`ivy--regex-plus`是`ivy`的默认补全方式，它是这样工作的：

例如用户搜索的是`"foo bar"`, 它会将`"foo bar"`里的多个连续的空格中的第1个空格转
换成正则里的通项匹配，即正则表达式`foo.*bar`。如果用户想匹配空格，那么需要额外的
多输入一个空格。如想匹配`"foo bar"`, 那么用户实际应该输入的是`foo  bar`(注意这里
要有2个空格).`ivy`会把它转换为`foo .*bar`(注意`foo`后面有个空格)。论坛会压缩空
格，效果下文见图片。

![ivy--regex-plus](https://emacs-china.org/uploads/default/original/2X/e/e3feb87f5411ea6ab9d3882e188b57159e918216.png)

此外它还支持正则的取非操作，通过`!`来完成。

例如`"foo bar !example"`会先得到匹配`foo.*bar`的结果，然后再将结果里匹配
`example`的候选项删除。

### ivy--regex

弱化版的`ivy--regex-plus`，不支持取非操作。

### ivy--regex-ignore-order

如名字所示，它会忽略正则的顺序，就好比多次`grep`一样。

- `foo` 会匹配 `foo`
- `foo bar` 会匹配 `foo bar` `bar foo`
- `foo !bar` 会匹配 `foo baz` 但是不会 `foo bar` 和 `bar foo`
- `foo[0-9]` 会匹配 `foo1` `foo2` 等等后面是数字的情况

如果想要匹配`!`号，需要转义一下:

`foo\\!bar` 将会匹配 `foo!bar`

此外，空格也可以转义：

`foo\\ bar` 将会匹配 `foo bar`

### ivy--regex-fuzzy

它会在每个字符后面都插入`.*`, 如 `abc` 对应的正则是 `a.*b.*c.*`。

### regexp-quote

自带的，见[官方文档](https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Functions.html).

## 增加自己的 ivy action

以`counsel-find-file`为例，有如下场景：

如果想要在查找文件的同时，发现有些文件命名不合适，有些文件需要删除还有些文件没有
权限打开需要使用`root`。最好是在`find-file`里就能实现而不用再去调用`dired`。此时
可以为`counsel-find-file`添加`action`.

```elisp
(ivy-set-actions
  'counsel-find-file
  '(("d" delete-file "delete")
    ("r" rename-file "rename"))
    ("x" counsel-find-file-as-root "open as root"))
```

![custom-action](https://emacs-china.org/uploads/default/original/2X/2/2b93e615847880b512bf2fc0ee7d56f8d5fc1046.png)

