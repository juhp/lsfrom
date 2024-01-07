# lsfrom

`lsfrom` lists the files and dirs in a directory starting from
the given filename or the first file after that
if the file does not exist, using the current locale collation order.

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
/tmp
/usr
/var
```

It can be useful for continuing a script on the entries of a directory
after a failure, etc:

```shellsession
$ myscript.sh $(lsfrom next)
```

or only running a command on a subrange of files in a directory.

## Usage

`$ lsfrom --version`
```
1.0
```
`$ lsfrom -h`
```
List directories files starting from file

Usage: lsfrom [--version] [-s|--strict] [-A|--all] [-a|--after]
              [-u|--until LASTFILE] [-b|--before] STARTFILE

  lsfrom lists the files in a directory that follow from the given file

Available options:
  -h,--help                Show this help text
  --version                Show version
  -s,--strict              fail if specified file(s) do not exist
  -A,--all                 include hidden (dot) files
  -a,--after               files after STARTFILE [default: from STARTFILE]
  -u,--until LASTFILE      files until FILE
  -b,--before              files before LASTFILE (only affects --until)
```

## Requirements

It uses `ls` to list files with locale sorting
and requires the `-A` option to exclude `.` and `..` (ie coreutils).
It also uses `sort` when injecting missing marker files.
It has been tested on Linux.

## Installation

`stack install lsfrom` or `cabal install lsfrom`.

## Contribute or feedback
lsfrom is distributed with BSD license
and the project is hosted at <https://github.com/juhp/lsfrom>.
