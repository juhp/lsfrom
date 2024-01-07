# lsfrom

`lsfrom` lists the files and dirs in a directory starting from the first one
that matches the given filename prefix or the first filename after that
if there is no match, using the current locale collation order.

```shellsession
$ ls
A a B C
$ echo $(lsfrom a)
a B C
$ lsfrom a/
a
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

## Usage

`$ lsfrom --version`
```
1.0.0
```
`$ lsfrom -h`
```
List files from pattern

Usage: lsfrom [--version] [-s|--strict] [-A|--all] [-a|--after] 
              [-u|--until LASTFILE] [-b|--before] STARTFILE

  lsfrom lists the files in a directory that follow from the given pattern

Available options:
  -h,--help                Show this help text
  --version                Show version
  -s,--strict              fail if specified file(s) do not exist
  -A,--all                 include hidden (dot) files
  -a,--after               files after STARTFILE [default: from STARTFILE]
  -u,--until LASTFILE      files until FILE
  -b,--before              files before LASTFILE [default: until LASTFILE]
```

## Requirements

It uses system `ls` order to preserve locale collation
and requires ls with the `-A` option to exclude `.` and `..` (ie coreutils).
It also use `sort`.
It has been tested on Linux.

## Installation

`stack install lsfrom` or `cabal install lsfrom`.

## Contribute or feedback
lsfrom is distributed with BSD license
and the project is hosted at <https://github.com/juhp/lsfrom>.
