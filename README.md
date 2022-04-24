# lsfrom

`lsfrom` lists the files and dirs in a directory starting from the first one
that matches the given filename prefix or the first filename after that
if there is no match, using the current locale collation order.

```
$ ls
A a B C
$ lsfrom a
a
B
C
$ lsfrom a/
B
C
$ lsfrom /sy
/sys
/sysroot
/tmp
/usr
/var
```

This may be useful for continuing a script on the content of a directory
after a failure, etc:

```shellsession
$ myscript.sh $(lsfrom next)
```

## Requirements

It uses system `ls` order to preserve locale collation
and requires ls with the `-A` option to exclude `.` and `..` (eg coreutils).
It has been tested on Linux.

## Installation

`stack install lsfrom` or `cabal install lsfrom`.

## Contribute or feedback
lsfrom is distributed with BSD license
and the project is hosted at <https://github.com/juhp/lsfrom>.
