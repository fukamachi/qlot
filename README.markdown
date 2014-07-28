# qlot

Qlot is a project-local library installer using Quicklisp facility. This aims to be like [Bundler](http://bundler.io) of Ruby or [Carton](http://search.cpan.org/~miyagawa/Carton/lib/Carton.pm) of Perl.

<strong><span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.</strong>

## Usage

```
# "qlfile" of "myapp"
ql :all :latest
git clack https://github.com/fukamachi/clack.git
git shelly https://github.com/fukamachi/datafly.git :branch v0.7.x
ql log4cl 2014-03-17
```

```common-lisp
(ql:quickload :qlot)
(qlot:install :myapp)
```

You can use it from a terminal with [Shelly](http://shlyfile.org/).

```
$ cd /path/to/myapp/
$ shly -Lqlot install
```

## What qlot is going to solve

We have Quicklisp, the central library registry. It made installation of libraries damn easy.

However, since what only you can specify is the month of distribution, you have to use all libraries at the same moment. You cannot use a newer/older version of a library for your project.

"local-projects/" or ASDF configurations may be a solution to this problem, but these are not project-local. If you have multiple projects that use the different version of the same library, it would be a problem.

Qlot is going to solve this problem.

## Tutorial

1. Put "qlfile" at the root of your project directory.
2. `(qlot:install :myapp)` or `shly -Lqlot install`.
3. Load the system by `qlot:quickload`.

## `qlfile` syntax

"qlfile" is a collection of Quicklisp dist declarations. Each line of that represents a dist.

```
<source> <dist name> [arg1, arg2..]
```

Currently, `<source>` must be one of `ql` or `git`.

### ql

`ql` source will download libraries from Quicklisp official dist, but you can specify the version.

If you want to use Clack in Quicklisp dist of January 13, 2014, qlfile would be like this.

```
ql clack 2014-01-13
```

`ql` source also allows `:all` as `<dist name>` and `:latest` as the version.

```
ql :all :latest
```

### git

`git` source will download libraries from a public git repository.

```
git clack https://github.com/fukamachi/clack.git
```

You can also specify `:ref`, `:branch` or `:tag`.

```
git clack https://github.com/fukamachi/clack.git :branch develop
git datafly https://github.com/fukamachi/datafly.git :tag v0.7.4
git cl-dbi https://github.com/fukamachi/cl-dbi.git :ref 54928984e5756e92ba298aae51de8b95a6b0cf4b
```

### Priorities

If multiple distributions provide the same library, lower one would take priority over higher ones.

## Installing other place than ./quicklisp/

```common-lisp
(qlot:install :myapp :quicklisp-home #P".ql/")
```

Set `:quicklisp-home` of your system.

```common-lisp
(defsystem myapp
  :defsystem-depends-on (:qlot-asdf)
  :class :qlot-system
  :quicklisp-home #P".ql/"
  :components ((:file "src/myapp")))
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
