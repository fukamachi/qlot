# Qlot

[![Build Status](https://github.com/fukamachi/qlot/workflows/CI/badge.svg)](https://github.com/fukamachi/qlot/actions)

**Qlot** (pronounced `kyü-'lät`, like culotte) is a project-local library installer using Quicklisp facility. This aims to be like [Bundler](http://bundler.io) of Ruby or [Carton](http://search.cpan.org/~miyagawa/Carton/lib/Carton.pm) of Perl.

## Table of Contents

* [Usage](#usage)
* [What Qlot is trying to solve](#what-qlot-is-trying-to-solve)
* [Requirements](#requirements)
* [Installation](#installation)
  * [Automatic installer](#automatic-installer)
  * [via Roswell](#via-roswell)
  * [via Quicklisp](#via-quicklisp)
  * [Install from source](#install-from-source)
  * [via Docker](#via-docker)
* [Optional settings](#optional-settings)
  * [ASDF configuration to prevent from loading by mistake](#asdf-configuration-to-prevent-from-loading-by-mistake)
* [Tutorial](#tutorial)
* [qlfile syntax](#qlfile-syntax)
* [Priorities of distributions](#priorities-of-distributions)
* [Working with SLIME](#working-with-slime)
  * [Lem](#lem)
  * [Emacs](#emacs)
  * [Vim/Neovim](#vimneovim)
* [Working with local git repositories](#working-with-local-git-repositories)

## Usage

```
# "qlfile" of "myapp"
git clack https://github.com/fukamachi/clack.git
github fukamachi/datafly :branch v0.7.x
ql log4cl 2014-03-17
ql mito :upstream
```

```
$ cd /path/to/myapp

# Installing libraries project-locally.
$ qlot install

# Updating depending libraries of a project.
$ qlot update

# Updating specific libraries
$ qlot update --project mito

# Execute a command with a project-local Quicklisp
$ qlot exec ros -S . run
$ qlot exec clackup app.lisp
```

## What Qlot is trying to solve

We have Quicklisp, the central library registry. It made installation of libraries damn easy.

However, what only you can specify is the month of distribution. Which means you have to use all libraries of the same moment and you cannot use a newer/older version of a library for your project.

"local-projects/" or ASDF configurations may be a solution to this problem, but there are a couple of problems.

1) *They are not project-local.* If you have multiple projects that use the different version of the same library, it would be a problem.

2) *They are difficult to fix the version or to update them.* If your project need to work on other than your machine, for instance on other people's machine or on servers, the version of depending libraries should be the same.

This is what Qlot is trying to solve.

## Requirements

* [Roswell](https://github.com/roswell/roswell/) or [SBCL](https://www.sbcl.org/)
* OpenSSL (Unix only)
  * **[Ubuntu/Debian]** `apt install libssl-dev`
  * **[macOS]** `brew install openssl`
* git (for installation from git repositories)

## Installation

### Automatic installer

```shell
$ curl -L https://qlot.tech/installer | bash
```

It requires `curl` and `sbcl` (or `ros`).

### via Roswell

If you're already using Roswell, this is the easiest way to install Qlot.

```shell
$ ros install qlot              # Install from the Quicklisp dist
$ ros install fukamachi/qlot    # Install the latest version from the git repository
```

Roswell adds an executable script under `$HOME/.roswell/bin`. Make sure if the directory exists in `$PATH`.

```shell
$ which qlot
/Users/fukamachi/.roswell/bin/qlot
```

Run `ros update qlot` to update Qlot.

### via Quicklisp

If [Quicklisp](https://www.quicklisp.org/) is set up on your home directory, this is the best way to install Qlot.

```shell
$ sbcl --noinform --eval '(ql:quickload (list :qlot :qlot/distify))' --quit
$ sudo printf '#!/bin/sh\nexec sbcl --noinform --non-interactive --eval "(ql:quickload :qlot/cli :silent t)" --eval "(qlot/cli:main)" "$@"\n' > /usr/local/bin/qlot
$ sudo chmod u+x /usr/local/bin/qlot
```

To update Qlot, run `(ql:update-all-dists)` in the REPL.

### Install from source

If you don't use both of Roswell and Quicklisp for some reason, Qlot also can be installed from the source code.

```shell
$ git clone https://github.com/fukamachi/qlot
$ cd qlot
$ scripts/setup.sh
$ sudo printf '#!/bin/sh\nexec '`pwd`'/scripts/run.sh "$@"\n' > /usr/local/bin/qlot
$ sudo chmod u+x /usr/local/bin/qlot
```

To update Qlot, run `git pull && scripts/setup.sh`.

**WARNING**: Don't add the Qlot source directory to any ASDF loadable directories, such as `~/common-lisp` or `~/quicklisp/local-projects`. ASDF accidentally loads dependencies of Qlot in a REPL even in case you don't need it. (See also [ASDF configuration](#asdf-configuration-to-prevent-from-loading-by-mistake) section)

### via Docker

[![Docker Pulls](https://img.shields.io/docker/pulls/fukamachi/qlot.svg)](https://hub.docker.com/r/fukamachi/qlot/)

```shell
$ docker run --rm -it fukamachi/qlot
```

## Optional settings

### ASDF configuration to prevent from loading by mistake

ASDF loads any ASD files under a directory `~/common-lisp` including its subdirectories. It is easily understandable and convenient. However, it will lead a problematic situation that ASDF accidentally loads libraries under ".qlot/" even in case you don't need it.

To avoid the situation, we recommend not to use `~/common-lisp` directory, or add the following lines to your Lisp's init file such as `~/.sbclrc` for SBCL.

```
;; .sbclrc
(push ".qlot" asdf::*default-source-registry-exclusions*)
(asdf:initialize-source-registry)
```

Roswell doesn't require this setting since it ignores directories starting with a dot.

## Tutorial

### Adding "qlfile"

Put a file "qlfile" at the root of your project directory.

See [qlfile syntax](#qlfile-syntax) section to know how to write it.

### Installation of libraries

You can install libraries into the project directory via:

```
$ qlot install
```

It creates `.qlot/` directory in the project directory and a file `qlfile.lock`.

`qlfile.lock` is similar to `qlfile` except the library versions are qualified. This will ensure that other developers or your deployment environment use exactly the same versions of libraries you just installed.

Make sure you add `qlfile` and `qlfile.lock` to your version controlled repository and make the `.qlot/` directory ignored.

```
$ echo .qlot/ >> .gitignore
$ git add qlfile qlfile.lock
$ git commit -m 'Start using Qlot.'
```

### Updating the project-local Quicklisp

You can update the content of `.qlot/` directory via:

```
$ qlot update

# Update a specific project
$ qlot update --project mito
$ qlot update --project mito,sxql
```

It will also overwrite `qlfile.lock`.

## Commands

### install

`qlot install` will install Quicklisp and libraries that declared in `qlfile` project-locally. `qlfile.lock` will be used with precedence if it exists.

```
$ qlot install
$ qlot install /path/to/myapp/qlfile
```

### update

`qlot update` will update the project-local `.qlot/` directory using `qlfile`.

```
$ qlot update

# Update a specific project
$ qlot update --project mito
$ qlot update --project mito,sxql
```

### add

`qlot add` will add a line to `qlfile` and invoke `qlot install` internally.
Its arguments are same as the qlfile syntax.

```
$ qlot add mito                          # ql mito
$ qlot add ql mito                       # ql mito (Same as the above)
$ qlot add ultralisp egao1980-cl-idna    # ultralisp egao1980-cl-idna
$ qlot add github fukamachi/datafly      # github datafly fukamachi/datafly
```

### exec

`qlot exec` does following:

* configures ASDF's source registries;
* adds Roswell's `bin` directory to the `PATH` environment variable;
* executes given command with arguments.

Here are few usefull commands:

* `qlot exec ros emacs` - starts Emacs for development. Inferior lisp will use only
  systems, installed by `qlot install`. If you want to use systems from directories other than
  current and `./.qlot/`, then set `CL_SOURCE_REGISTRY` variable before starting `qlot`.
  This can be useful in case, if you have development versions of some systems, for example,
  in `~/common-lisp/` directory and want to use them during project development:

  ```
  CL_SOURCE_REGISTRY=~/common-lisp// qlot exec ros emacs
  ```

  Read more about `CL_SOURCE_REGISTRY` in
  [asdf's documentation](https://common-lisp.net/project/asdf/asdf/Shell_002dfriendly-syntax-for-configuration.html).
* `qlot exec ros build some-app.ros` - another command, useful, to build a binary
  from systems, fixed in `qlfile` and `qlfile.lock`. This way you can be sure that your builds are stable.

**NOTE**: `qlot exec` is only affects to `ros` or Roswell scripts.

If you're using Qlot without Roswell, load `.qlot/setup.lisp` instead, like:

```
$ sbcl --noinform --no-userinit --no-sysinit --load .qlot/setup.lisp
```

## `qlfile` syntax

"qlfile" is a collection of Quicklisp dist declarations. Each line of that represents a dist.

```
<source> <project name> [arg1, arg2..]
```

Currently, `<source>` must be one of `dist`, `ql`, `ultralisp`, `http`, `git` or `github`.

### ql

```
ql <project name> <version>
ql <project name>
```

`ql` source will download libraries from Quicklisp official dist.

If you want to use Clack in Quicklisp dist of January 13, 2014, qlfile would be like this.

```
ql clack 2014-01-13
```

If `:latest` is specified for the version, the latest Quicklisp dist version will be used.

If `:upstream` is specified, Qlot downloads the latest code from the upstream git repository.

### ultralisp

```
ultralisp <project name> <version>
ultralisp <project name>
```

`ultralisp` is same as `ql` except downloading from Ultralisp.

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

#### Retrieving from private repository

Qlot doesn't authenticate itself, but retrieving from private repository can be done via git's SSH key authentication. Which means, if the current user can `git clone`, Qlot also would be possible to do it.

```
git myapp git@github.com:somewrite-adtech/myapp
```

### github

```
github <username/repository>
github <username/repository> :ref <commit ref>
github <username/repository> :branch <branch name>
github <username/repository> :tag <tag name>
```

`github` source is similar to `git`, but it is specifically for GitHub. As it uses GitHub API and tarballs GitHub serves, it doesn't require "git" command.

```
github fukamachi/datafly
github fukamachi/datafly :branch develop
```

### dist

```
dist <dist name> <distribution URL> [<dist version>]
```

`dist` allows to use a custom Quicklisp dist, like Ultralisp.

```
dist quicklisp http://beta.quicklisp.org/dist/quicklisp.txt
dist ultralisp http://dist.ultralisp.org/
```

## Priorities of distributions

If multiple distributions provide the same library, lower one would take priority over higher ones.

## Working with SLIME

[SLIME](https://github.com/slime/slime) is the most popular development environment in Common Lisp. However, its REPL always loads the global Quicklisp, not the project-local one.

Here's quick steps to start project-local REPL with SLIME for each text editors:

### Lem

1. Add [lem-project/micros](https://github.com/lem-project/micros) to `.qlot/local-projects`.

```shell
$ git clone https://github.com/lem-project/micros .qlot/local-projects/micros
```

2. Add the following function to `~/.lem/init.lisp`.

```common-lisp
(define-command slime-qlot-exec (directory) ((prompt-for-directory (format nil "Project directory (~A): " (buffer-directory))))
  (let ((command (first (lem-lisp-mode/implementation::list-roswell-with-qlot-commands))))
    (when command
      (lem-lisp-mode:run-slime command :directory directory))))
```

3. Relaunch the Lem, or reload the init file with `M-x lisp-load-file RET ~/.lem/init.lisp`.

4. Invoke `M-x slime-qlot-exec RET /path/to/project/`.

### Emacs

1. Add one of the following functions to `init.el`.

#### a) SBCL version

```emacs-lisp
(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "sbcl"
               :program-args `("--no-userinit" "--no-sysinit" "--load" ,(concat (file-name-as-directory directory) ".qlot/setup.lisp"))
               :directory directory
               :name 'qlot
               :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))
```

#### b) Roswell version

```emacs-lisp
(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))
```

2. Relaunch the Emacs.
3. Invoke `M-x slime-qlot-exec RET /path/to/project/`.

### Vim/Neovim

1. Install [vlime/vlime](https://github.com/vlime/vlime).
2. Add the following code to your Vim/Neovim init.vim.

```vimscript
let g:vlime_cl_use_terminal = v:true
let s:vlime_path = '/path/to/vlime'
function! VlimeBuildServerCommandFor_qlot(vlime_loader, vlime_eval)
    return ["qlot", "exec", "ros", "run",
               \ "--load", s:vlime_path . "/lisp/load-vlime.lisp",
               \ "--eval", vlime_eval]
endfunction
function! VlimeQlotExec()
    call vlime#server#New(v:true, get(g:, "vlime_cl_use_terminal", v:false), v:null, "qlot")
endfunction
```

3. Relaunch the Vim/Neovim.
4. Change the directory by `:cd /path/to/project/` and invoke `:call VlimeQlotExec()`.

## Working with local git repositories

`PROJECT_ROOT/.qlot/local-projects` can be used for local git repositories. Symbolic links are also be accessible in Qlot environment.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## License

See [LICENSE.txt](LICENSE.txt).

The license of bundled Quicklisp installer can be found at [quicklisp/LICENSE.txt](quicklisp/LICENSE.txt).
