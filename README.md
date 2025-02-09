# lsfrom

`lsfrom` lists the files and dirs in a directory starting from
the given filename or the first file after that
if the file does not exist, using the current locale collation order.

```shellsession
$ ls
A a B C
$ echo $(lsfrom --from a)
a B C
$ lsfrom -f a/
a
B
C
$ lsfrom --until B
A
a
B
$ lsfrom -f /sy
/sys
/tmp
/usr
/var
```

It can be useful for continuing a script on the entries of a directory
after a failure, etc:

```shellsession
$ myscript.sh $(lsfrom -f next)
```

or only running a command on a subrange of files in a directory.

## Usage

`$ lsfrom --version`

```
1.1
```
`$ lsfrom -h`

```
List directories files starting from file

Usage: lsfrom [--version] [-A|--all]
              [(-f|--from STARTFILE) | (-a|--after STARTFILE)]
              [(-u|--until LASTFILE) | (-b|--before LASTFILE)]

  lsfrom lists the files in a directory that follow from the given file

Available options:
  -h,--help                Show this help text
  --version                Show version
  -A,--all                 include hidden (dot) files
  -f,--from STARTFILE      files from STARTFILE
  -a,--after STARTFILE     files after STARTFILE
  -u,--until LASTFILE      files until LASTFILE
  -b,--before LASTFILE     files before LASTFILE
```

## Requirements

It uses `ls` to list files with locale sorting
and requires the `-A` option to exclude `.` and `..` (ie coreutils).
It has been tested on Linux.

## Installation

`stack install lsfrom` or `cabal install lsfrom`.

## Contribute or feedback
lsfrom is distributed with BSD3 license
and the project is hosted at <https://github.com/juhp/lsfrom>.
