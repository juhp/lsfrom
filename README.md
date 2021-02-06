# lsfrom

`lsfrom` lists files in the current directory that start with a
particular sequence of characters and those after it with respect to
collation.

```
$ ls
A a B C
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

This may be useful for continuing a script on the files in a dir
after a failure, etc.
