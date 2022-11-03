---
title: "Use remote language server on Tramp on emacs"
date: 2022-10-31 13:00:00 +0200
categories: tool
tags: emacs tramp
---

In my lecture I need the ssh access and write exercises there because my own laptop does not have the required environments.
I am using Doom emacs, so with the module `upload` (i.e., `Tramp`) enabled, I can open remote files with `SPC f f /ssh:<remote-server>:` where the `<remote-server>` can be pre-confifured in `~/.ssh/config`.
The problem is that when working on remote files I cannot use my local language server such as `pyright`.

To also enable this lsp support, I did the following things:

1. Install the language server, say `pylsp` on the remote server with `pip install python-lsp-server` (if the pip version is too old, it may fail and one needs to upgrade pip first).

2. Check the path where `pylsp` is located on the remote server with `which pylsp`. In my case it is in `~/.local/bin`.

3. In the `config.el` of my local Doom emacs, I add the following configs in the ending:

```lisp
(require 'lsp-mode)
(lsp-register-client
(make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                :major-modes '(python-mode)
                :remote? t
                :server-id 'pylsp-remote))

(require 'tramp)
(add-to-list 'tramp-remote-path "/home/<usr>/.local/bin") ;; replace user
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
```

This is not an optimal emacs config, as with `require` I may always enable these modes, but if I use `after!` there will be some issues, so I will for now live with this.

The first part tells emacs that when accessing remote python files, it should use `pylsp` server in the `lsp-mode`.
The second part tells emacs where to find `pylsp` on the remote server, as the path in my case is not by default configured in the Tramp.

Finally, check the config is correct and restart emacs, then when accesing remote python files `pylsp` shoule work fine.
A problem with this approach is that sometimes the emacs will stuck due to unresponding Tramp (by my understanding), I haven't found a way to solve this except force quiting and restarting emacs.
