# lsfrom

`lsfrom` lists files in the current directory that start with a
particular sequence of characters and those after it with respect to
collation.

eg `$ cd /; echo $(lsfrom sy)` outputs "sys tmp usr var" for me.

This may be useful for continuing a script on the files in a dir
after a failure, etc.
