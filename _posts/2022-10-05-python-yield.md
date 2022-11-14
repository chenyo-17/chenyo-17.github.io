---
title: "Python hints"
date: 2022-10-05 12:00:00 +0200
categories: lang
tags: python
math: true
---

- `yield` and `yield from`

    [This blog post](http://simeonvisser.com/posts/python-3-using-yield-from-in-generators-part-1.html) gives a good introduction of how to use `yield` and `yield from` in Python.

- add local path with conda

    Similar to a normal IDE where a project root is required to import local modules, with emacs and lsp, I first need to include the path in the environment.
    To check current local paths, open a Python interpreter and use 

    ```python
    import sys
    for p in sys.path:
        print(p)
    ```

    To add a path with conda, use `conda develop <path>` inside the environment, note that redundant paths should be avoided, e.g., if both `~/src/` and `~/src/modulea` are present in the paths, there could be a module resolvation issue. 
    After adding the path, need to activate conda environment and ananconda mode in emacs.
    To remove a path, use `conda develop --uninstall <path>`.

- pdb

    `pdb` is a python debugger in emacs, to start `pdb` I first need to be in the buffer of the specific file, the keywords are similar to gdb: `s` goes into a function, `n` skips entering inner functions, `b` lists current breakpoints and `b <line-number>` set a breakpoint at the given line, `c` continues the execution until reaching a breakpoint.
Sometimes, there is error in the python interpreter but works fine in `pdb`, restart the python interpreter process would likely solve the problem.








