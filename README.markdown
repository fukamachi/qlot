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

### Adding "qlfile"

Put a file "qlfile" at the root of your project directory.

See [qlfile syntax](#qlfile-syntax) section to know how to write it.

### Installation of libraries

You can install libraries into the project directory via:

```common-lisp
(qlot:install :myapp)
```

It creates `quicklisp/` directory in the project directory and a file `qlfile.snapshot`.

`qlfile.snapshot` is similar to `qlfile` except the library versions are qualified. This will ensure that other developers or your deployment environment use exactly the same versions of libraries you just installed.

Make sure you add `qlfile` and `qlfile.snapshot` to your version controlled repository and make the `quicklisp/` directory ignored.

```
$ echo quicklisp/ >> .gitignore
$ git add qlfile qlfile.snapshot
$ git commit -m 'Start using qlot.'
```

### Loading your application

To load your qlot-ready application, use `qlot:quickload` instead of `ql:quickload`.

```common-lisp
(qlot:quickload :myapp)
```

### Updating the project-local quicklisp

You can update the content of `quicklisp/` directory via:

```common-lisp
(qlot:update :myapp)
```

It will also overwrite `qlfile.snapshot`.

### Deploying your application

`qlot:install` will use `qlfile.snapshot` if it exists.

```common-lisp
(qlot:install :myapp)
```

## `qlfile` syntax

"qlfile" is a collection of Quicklisp dist declarations. Each line of that represents a dist.

```
<source> <dist name> [arg1, arg2..]
```

Currently, `<source>` must be one of `ql`, `http`, `git` or `github`.

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

### http

`http` source will download a tarball.

```
http yason http://netzhansa.com/yason.tar.gz
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

### github

`github` source is similar to `git`, but it is specifically for GitHub. As it uses GitHub API and tarballs GitHub serves, it doesn't require "git" command.

```
github datafly fukamachi/datafly
github datafly fukamachi/datafly :branch develop
```

### Priorities of distributions

If multiple distributions provide the same library, lower one would take priority over higher ones.

## Installing to other place than ./quicklisp/

Set `:quicklisp-home` of your system and `(qlot:install :myapp)`.

```common-lisp
(defsystem myapp
  :defsystem-depends-on (:qlot)
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
