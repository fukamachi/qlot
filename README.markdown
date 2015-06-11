# qlot

[![Circle CI](https://circleci.com/gh/fukamachi/qlot.svg?style=svg)](https://circleci.com/gh/fukamachi/qlot)

Qlot is a project-local library installer using Quicklisp facility. This aims to be like [Bundler](http://bundler.io) of Ruby or [Carton](http://search.cpan.org/~miyagawa/Carton/lib/Carton.pm) of Perl.

<strong><span style="color:red">Warning</span>: This software is still ALPHA quality. The APIs will be likely to change.</strong>

## Usage

```
# "qlfile" of "myapp"
git clack https://github.com/fukamachi/clack.git
github datafly fukamachi/datafly :branch v0.7.x
ql log4cl 2014-03-17
```

```common-lisp
;; Installing libraries project-locally.
(qlot:install :myapp)

;; Loading a project with its project-local quicklisp.
(qlot:quickload :myapp)

;; Updating depending libraries of a project.
(qlot:update :myapp)
```

## What qlot is trying to solve

We have Quicklisp, the central library registry. It made installation of libraries damn easy.

However, what only you can specify is the month of distribution. Which means you have to use all libraries of the same moment and you cannot use a newer/older version of a library for your project.

"local-projects/" or ASDF configurations may be a solution to this problem, but there are a couple of problems.

1) *They are not project-local.* If you have multiple projects that use the different version of the same library, it would be a problem.

2) *They are difficult to fix the version or to update them.* If your project need to work on other than your machine, for instance on other people's machine or on servers, the version of depending libraries should be the same.

This is what qlot is trying to solve.

## Installation

### via Quicklisp

As [qlot is going to be included](https://github.com/quicklisp/quicklisp-projects/issues/716) in Quicklisp dist in August 2014, you can install it through Quicklisp.

```common-lisp
(ql:quickload :qlot)
```

## Tutorial

### Adding "qlfile"

Put a file "qlfile" at the root of your project directory.

See [qlfile syntax](#qlfile-syntax) section to know how to write it.

### Installation of libraries

You can install libraries into the project directory via:

```common-lisp
(qlot:install :myapp)
```

It creates `quicklisp/` directory in the project directory and a file `qlfile.lock`.

`qlfile.lock` is similar to `qlfile` except the library versions are qualified. This will ensure that other developers or your deployment environment use exactly the same versions of libraries you just installed.

Make sure you add `qlfile` and `qlfile.lock` to your version controlled repository and make the `quicklisp/` directory ignored.

```
$ echo quicklisp/ >> .gitignore
$ git add qlfile qlfile.lock
$ git commit -m 'Start using qlot.'
```

### Loading your application

To load your qlot-ready application, use `qlot:quickload` instead of `ql:quickload`.

```common-lisp
(qlot:quickload :myapp)
```

### Executing forms with project-local Quicklisp

Although `qlot:quickload` loads a project with its project-local Quicklisp, the Quicklisp path will be restored to the default one after that.

This could cause significant problem if your application loads other libraries during run-time.

For example, [Clack](http://clacklisp.org/) loads a server handler when executing `clackup` and, the important part is, it loads with the system default Quicklisp, not the project-local one.

To prevent the mess, wrap all code which would load other libraries in run-time with `qlot:with-local-quicklisp`.

```common-lisp
(qlot:with-local-quicklisp :myapp
  (clack:clackup *app* :server :wookie))
```

### Updating the project-local quicklisp

You can update the content of `quicklisp/` directory via:

```common-lisp
(qlot:update :myapp)
```

It will also overwrite `qlfile.lock`.

### Bundling libraries

You can bundle all depending libraries by adding the project-local `quicklisp/` directory to version controlled repository.

```common-lisp
(qlot:install :myapp)
```

```
$ echo quicklisp/cache >> .gitignore
$ echo quicklisp/tmp >> .gitignore
$ git add .gitignore quicklisp/
$ git commit -m 'Bundle dependencies.'
```

## Commands

### install

`qlot:install` will install Quicklisp and libraries that declared in `qlfile` project-locally. `qlfile.lock` will be used with precedence if it exists.

```common-lisp
(qlot:install :myapp)
(qlot:install #P"/path/to/myapp/")
(qlot:install #P"/path/to/myapp/my-qlfile")
```

### update

`qlot:update` will update the project-local `quicklisp/` directory using `qlfile`.

```common-lisp
(qlot:update :myapp)
(qlot:update #P"/path/to/myapp/")
(qlot:update #P"/path/to/myapp/my-qlfile")
```

### quickload

`qlot:quickload` is similar to `ql:quickload` except it uses its project-local `quicklisp/` directory.

```common-lisp
(qlot:quickload :myapp)
```

### with-local-quicklisp

Eval the given form in the local quicklisp environment.

```common-lisp
(qlot:with-local-quicklisp :myapp
  (ql:quickload :drakma))
```

## `qlfile` syntax

"qlfile" is a collection of Quicklisp dist declarations. Each line of that represents a dist.

```
<source> <project name> [arg1, arg2..]
```

Currently, `<source>` must be one of `ql`, `http`, `git` or `github`.

### ql

```
ql <project name> <version>
ql <project name> :latest
ql :all <version>
```

`ql` source will download libraries from Quicklisp official dist, but you can specify the version.

If you want to use Clack in Quicklisp dist of January 13, 2014, qlfile would be like this.

```
ql clack 2014-01-13
```

`ql` source also allows `:all` as `<dist name>` and `:latest` as the version.

```
ql :all 2014-01-13
ql clack :latest
```

### http

```
http <project name> <url> [<file md5>]
```

`http` source will download a tarball.

```
http yason http://netzhansa.com/yason.tar.gz
```

### git

```
git <project name> <repos url>
git <project name> <repos url> :ref <commit ref>
git <project name> <repos url> :branch <branch name>
git <project name> <repos url> :tag <tag name>
```

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

```
github <project name> <repos>
github <project name> <repos> :ref <commit ref>
github <project name> <repos> :branch <branch name>
github <project name> <repos> :tag <tag name>
```

`github` source is similar to `git`, but it is specifically for GitHub. As it uses GitHub API and tarballs GitHub serves, it doesn't require "git" command.

```
github datafly fukamachi/datafly
github datafly fukamachi/datafly :branch develop
```

### Priorities of distributions

If multiple distributions provide the same library, lower one would take priority over higher ones.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
