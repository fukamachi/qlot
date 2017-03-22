# Shelly

## Usage

    Usage: shly [option,..] <command> [arg1,arg2..]

    $ shly -Lclack clackup /path/to/project/app.lisp
    $ shly -Ldrakma http-request http://www.hatena.com/

## Description

Shelly enables you to execute Common Lisp functions like a shell command. And it also can be used as a Make-like build-tool.

Shelly has the following features:

* Provides shell command-like interface for Common Lisp functions
* Dumps a Lisp core for fast execution.
* Allows to define project specific commands. (shlyfile)
* Implementation independent.

<strong><span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.</strong>

## Why use it?

In Common Lisp world, most libraries and applications are designed to run on REPL. It is convenient for interactive development, but it would be an obstacle in some cases.

For example, Common Lisp isn't good for writing a small script. No common way to write it in a portable way. Parsing command-line arguments is really annoying. And, the startup time would be very slow, especially when it uses some libraries.

Shelly is trying to solve these problems.

### 1. Implementation independent

Shelly works with SBCL, Clozure CL, Allegro CL, ABCL, GNU CLISP and ECL.

### 2. Function as a shell command

Shelly treats general functions as its sub-commands, so you don't even need to write a script in most cases.

    (in-package :myapp)
    
    (defun do-something (&key verbose)
      ;; Do something.
      )

    $ shly myapp:do-something --verbose t

Command-line options and arguments will be delivered to a function.

### 3. Fast startup time

Shelly reduces the startup time by storing a Lisp core image. In a simple case, the execution is about 10 times faster than CIM's `cl` command and even 9 times faster than SBCL (with Quicklisp) at the maximum.

    # Uses SBCL v1.2.1, Shelly v0.7.0
    $ time shly + 1 2
    3
    shly + 1 2  0.05s user 0.03s system 96% cpu 0.086 total
    
    # CIM v1.0.0
    $ time cl --eval '(princ (+ 1 2))'
    3
    cl --eval '(princ (+ 1 2))'  1.23s user 0.37s system 99% cpu 1.597 total
    
    # SBCL with Quicklisp
    $ time sbcl --noinform --eval '(princ (+ 1 2))' --eval '(quit)'
    3
    sbcl --noinform --eval '(princ (+ 1 2))' --eval '(quit)'  1.12s user 0.37s system 99% cpu 1.499 total
    
    # SBCL without Quicklisp
    $ time sbcl --noinform --no-userinit --eval '(princ (+ 1 2))' --eval '(quit)'
    3
    sbcl --noinform --no-userinit --eval '(princ (+ 1 2))' --eval '(quit)'  0.01s user 0.01s system 87% cpu 0.014 total

## How does it work

Shelly provides a shell script "shly". It takes some options, a command, and arguments for the command.

    Usage: shly [option,..] <command> [arg1,arg2..]

In this example, `ql:system-apropos` would be the command and `web` would be an argument.

    ;; Same as (ql:system-apropos "web")
    $ shly ql:system-apropos web
    #<SYSTEM bknr.modules / bknr-web-20140616-git / quicklisp 2014-06-16>
    #<SYSTEM bknr.web / bknr-web-20140616-git / quicklisp 2014-06-16>
    #<SYSTEM cl-web-crawler / cl-web-crawler-20130128-svn / quicklisp 2014-06-16>
    #<SYSTEM cl-webdav / cl-webdav-0.2.1 / quicklisp 2014-06-16>
    #<SYSTEM crane-web / crane-20140616-git / quicklisp 2014-06-16>
    #<SYSTEM hh-web / hh-web-20140616-git / quicklisp 2014-06-16>
    ...

If an argument starts with ":", it would be converted into a keyword.

    ;; Same as (asdf:system-source-file :hunchentoot).
    $ shly asdf:system-source-file :hunchentoot
    #P"/Users/nitro_idiot/quicklisp/dists/quicklisp/software/hunchentoot-1.2.21/hunchentoot.asd"

If an argument starts with "--", it also would be a keyword. This is just like common shell commands.

    ;; Same as (ql:update-all-dists :prompt nil)
    $ shly ql:update-all-dists --prompt nil

If the command is imported to `COMMON-LISP-USER` package, you can omit the package prefix.

    $ shly list 1 2 3
    (1 2 3)

Or, if the package name is the same as the system name that is loaded by `-L` option, you can also omit it.

    $ shly -Ldrakma http-request http://www.hatena.com/
    $ shly -Lcl-project make-project /path/to/myapp/ --description "My sample app." --author "Eitaro Fukamachi"

## Declaring project specific commands

Shelly loads a local file which is named `shlyfile.lisp` if it exists. You can define project specific commands by writing functions in it. This is just like a "Makefile" in Common Lisp.

```common-lisp
;; shlyfile.lisp
(defun test ()
  (asdf:test-system :your-app))

(defun build ()
  ;; Somthing to build your app.
  )
```

Then, `shly test` and `shly build` would be available only in the directory.

```
$ shly test
$ shly build
```

Shelly also loads `~/.shelly/shlyfile.lisp` every time if it exists. If you have some commands you'd like to use everywhere, put them into that file.

## Installation

If you've installed [CIM](https://github.com/KeenS/CIM), Shelly will use its setting.

- [CIM: Common Lisp Implementation Manager](https://github.com/KeenS/CIM)

If not, you need at least one Common Lisp implementation and [Quicklisp](http://www.quicklisp.org/beta/).

### Installing from Quicklisp

The stable version is included in Quicklisp dist.

    (ql:quickload :shelly)
    (shelly:install)

or

    $ curl -L http://shlyfile.org/shly | /bin/sh

### Installing from source

```
$ git clone https://github.com/fukamachi/shelly.git
$ cd shelly
$ SHELLY_PATH=. bin/shly install
```

### Installing to other than ~/.shelly

If you want to install Shelly to the different location, set SHELLY_HOME to the directory path.

    $ curl -L http://shlyfile.org/shly | SHELLY_HOME=~/.shly /bin/sh

## Configuration

If you use bash, zsh, csh or tcsh, the initialization code will be appended into your .*rc file automatically.

Otherwise, set SHELLY_HOME and add ~/.shelly/bin to PATH manually.

    SHELLY_HOME="$HOME/.shelly"
    [ -s "$SHELLY_HOME/shelly/init.sh" ] && . "$SHELLY_HOME/shelly/init.sh"

## Upgrading

Shelly ver 0.5.0 or higher allows to specify `--version` to `install` command. You can choose the version from the output of `shly available-versions`

```
$ shly available-versions
v0.6.1
v0.6.0
v0.5.8
v0.5.7
v0.5.6
v0.5.5
v0.5.4
v0.5.3
v0.5.2
v0.5.1
v0.5.0
v0.4.2
v0.4.1
v0.4.0
v0.3.1
v0.3
v0.2
v0.1
```

You can install the latest version of Shelly by running the following command.

```
$ shly install --version latest
```

To use Shelly ver 0.6.1 or older versions, you need Perl5, a Common Lisp implementation and Quicklisp.

## Uninstalling

Just delete the ~/.shelly directory.

```
$ rm -rf ~/.shelly
```

## Built-in Commands

### help [&optional command]

Show the usage of the specified `command`.

    $ shly help ql:quickload
    Usage: ql:quickload (systems &key verbose prompt explain
                         (verbose *quickload-verbose*) (prompt *quickload-prompt*)
                         &allow-other-keys)
        Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
           of things to be loaded.

If `command` is not specified, it shows all available commands. This is the same as `shly --help`.

    $ shly help

### install [&key version]

Install Shelly into your environment under "~/.shelly". You can install a specific version by using "--version".

    $ shly install --version v0.6.1

### available-versions

Show all the possible Shelly versions.

    $ shly available-versions

### dump-core

Dump Lisp core image file for faster startup.

    $ shly dump-core

### rm-core

Remove saved core image file which created by `dump-core`.

    $ shly rm-core

### local-dump-core [&rest systems]   \(Experimental)

Dump Lisp core image file to the current directory. This command takes system names to be included in the core.

    $ shly local-dump-core :myapp

## History (roughly)

### v0.7.0

* Use CIM for CL implementation management if it is installed.
* Rewrite "bin/shly" in Shell script.
* Remove the dependency on SWANK.
* Allow to specify where to install with `SHELLY_HOME`.
* Add `upgrade` command.

### v0.6.0 (Mar 31, 2014)

* Add `local-dump-core` (experimental).
* Allow multiple `-L` options.
* Add a new option `-I` to add a directory path to `asdf:*central-registry*`.

### v0.5.8 (Nov 12, 2013)

* Muffle outputs of `ql:quickload` when `--verbose` isn't specified.

### v0.5.0 (Aug 29, 2013)

* Allow to install the specific version of Shelly.
* Show commands even in "shlyfile"s.

### v0.4.0 (Aug 26, 2013)

* Add "shlyfile" feature.

## Note: Conversion rules

### Number

    $ shly type-of 24
    (INTEGER 0 4611686018427387903)
    $ shly type-of 3.141592653589793d0
    DOUBLE-FLOAT

### Cons

    $ shly type-of '(list 1 2 3)'
    CONS

### Boolean

If it is "t" or "nil", it would be `T` or `NIL`.

    $ shly type-of t
    BOOLEAN
    $ shly type-of nil
    NULL

### Keyword

If it starts with ":" or "--", it would be a keyword.

    $ shly type-of :web
    KEYWORD
    $ shly type-of --verbose

### Pathname

If the same name file exists, it would be a pathname. Otherwise, it would be just a string.

    $ shly type-of test.lisp
    (SIMPLE-ARRAY CHARACTER (9))
    $ touch test.lisp
    $ shly type-of test.lisp
    PATHNAME

### Symbol

If the same name symbol exists, it would be a symbol.

    $ shly pi
    DOUBLE-FLOAT
    $ shly asdf:\*central-registry\*
    CONS

### String

Otherwise, it would be treated as a string.

    $ shly common-lisp-is-great
    (SIMPLE-ARRAY CHARACTER (20))

## Copyright

Copyright (c) 2012-2014 Eitaro Fukamachi and [contributors](https://github.com/fukamachi/shelly/graphs/contributors).

## License

Licensed under the BSD (2-Clause) License. See LICENSE.
