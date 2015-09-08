Tofu
====

I'm told to-do list apps are the new Hello World. This is a simple
to-do list app I'm playing with as I teach myself Clojure.

Usage
-----

```bash
$ lein run
```

Command Index
-------------

    ' --- load-opts-from-register-command
    " --- save-opts-to-register-command
    * --- toggle-priority-command
    + --- toggle-debug-command
    . --- cycle-sort-fn-command
    / --- toggle-regex-filter
    > --- toggle-reverse-sort-command
    ? --- help-command
    A --- toggle-show-ages-command
    D --- toggle-filter-done-command
    a --- add-task-command
    d --- delete-task-command
    e --- edit-task-command
    h --- help-command
    l --- clear-screen-command
    p --- print-command
    q --- quit-command
    r --- redo-command
    s --- save-command
    t --- toggle-done-command
    u --- undo-command

Project To-Do List
----------------

A to-do list for Tofu development lives within the project files.  To
view / edit it, run:

```sh
$ TOFU_FILE=todo.tofu lein run
```

**Author:** Cameron Desautels \<<camdez@gmail.com>\>  
**Source:** <http://github.com/camdez/tofu>
