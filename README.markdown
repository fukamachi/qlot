# Qlot

[![Build Status](https://github.com/fukamachi/qlot/workflows/CI/badge.svg)](https://github.com/fukamachi/qlot/actions)
[![](https://api.quickdocs.org/badge/qlot.svg)](https://quickdocs.org/qlot)

**Qlot** (pronounced `kyü-'lät`, like culotte) is a project-local library installer using Quicklisp facility. This aims to be like [Bundler](http://bundler.io) of Ruby or [Carton](http://search.cpan.org/~miyagawa/Carton/lib/Carton.pm) of Perl.

## Table of Contents

* [Usage](#usage)
* [What Qlot is trying to solve](#what-qlot-is-trying-to-solve)
* [Requirements](#requirements)
* [Installation](#installation)
  * [Automatic installer](#automatic-installer-recommended)
  * [via Roswell](#via-roswell)
  * [via Quicklisp](#via-quicklisp)
  * [Manual installation](#manual-installation)
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

### Shell

```
$ cd /path/to/myapp

# Initialize the project to start using Qlot.
$ qlot init

# Install libraries project-locally.
$ qlot install

# Add the upstream version of a library
$ qlot add mito --upstream

# Add a library from GitHub
$ qlot add fukamachi/anypool

# Update specific libraries
$ qlot update mito

# Run a REPL with a project-local Quicklisp
$ qlot exec sbcl
```

### REPL (experimental)

```common-lisp
;; Move to the project root
;; (not necessary if the REPL is invoked by 'qlot exec')
(setf qlot:*project-root* #P"/path/to/project/")

;; Initialize the project to start using Qlot.
(qlot:init #P"/path/to/project/")

;; Install libraries project-locally.
(qlot:install)

;; Add the upstream version of a library
(qlot:add :mito :upstream t)

;; Add a library from GitHub
(qlot:add "fukamachi/anypool")

;; Update specific libraries
(qlot:update :mito)
```

## What Qlot is trying to solve

We have Quicklisp, the central library registry. It made installation of libraries damn easy.

However, what only you can specify is the month of distribution. Which means you have to use all libraries of the same moment and you cannot use a newer/older version of a library for your project.

"local-projects/" or ASDF configurations may be a solution to this problem, but there are a couple of problems.

1) *They are not project-local.* If you have multiple projects that use different versions of the same library, it would be a problem.

2) *They are difficult to fix the version or to update them.* If your project needs to work on other than your machine, for instance on other people's machines or on servers, the version of depending libraries should be the same.

This is what Qlot is trying to solve.

## Requirements

* [Roswell](https://github.com/roswell/roswell/) or [SBCL](https://www.sbcl.org/)
* OpenSSL (Unix only)
  * **[Ubuntu/Debian]** `sudo apt install libssl-dev`
  * **[macOS]** `brew install openssl`
* git (for installation from git repositories)

## Installation

### Automatic installer (recommended)

```shell
$ curl -L https://qlot.tech/installer | sh
```

It also requires `curl` (or `wget`) and `tar`.

To uninstall Qlot, run a Qlot uninstaller like:

```
$ ~/.qlot/qlot/scripts/qlot-uninstaller.sh
```

### via Roswell

If you're already using Roswell, Qlot can be installed by `ros install`.

```shell
# Install from the Quicklisp dist
$ ros install qlot
# Or, install the latest version from the git repository
$ ros install fukamachi/qlot

# For older Roswell (Not required since v23.10.14.114 or above)
$ ros -e '(ql:quickload :qlot/distify)'
```

Roswell adds an executable script under `$HOME/.roswell/bin`. Make sure the directory exists in `$PATH`.

```shell
$ which qlot
/Users/fukamachi/.roswell/bin/qlot
```

Run `ros update qlot` to update Qlot.

### via Quicklisp

If [Quicklisp](https://www.quicklisp.org/) is set up on your home directory, Qlot can be installed by `ql:quickload`.

```common-lisp
(ql:quickload :qlot)
```

Use functions exported by `qlot` package for using Qlot.

To update Qlot, run `(ql:update-all-dists)` in the REPL.

### Manual installation

If you don't use both Roswell and Quicklisp for some reason, Qlot also can be installed manually.

The advantage of this method is no dependencies are required other than `sbcl` and `OpenSSL`.

```shell
# Getting the latest version
$ curl -sL https://api.github.com/repos/fukamachi/qlot/releases/latest | jq -rM '.name'
1.3.5
```

```shell
$ curl https://github.com/fukamachi/qlot/releases/download/1.3.5/qlot-1.3.5.tar.gz -o qlot.tar.gz
$ tar xfz qlot.tar.gz
$ cd qlot
$ scripts/setup.sh
$ sudo scripts/install.sh
```

### Install from source

```shell
$ git clone https://github.com/fukamachi/qlot
$ cd qlot
$ scripts/setup.sh
$ sudo scripts/install.sh
```

To update Qlot, run `git pull && scripts/setup.sh`.

**WARNING**: Don't add the Qlot source directory to any ASDF loadable directories, such as `~/common-lisp` or `~/quicklisp/local-projects`. ASDF accidentally loads dependencies of Qlot in a REPL even in case you don't need it. (See also [ASDF configuration](#asdf-configuration-to-prevent-from-loading-by-mistake) section)

### via Docker

[![Docker Pulls](https://img.shields.io/docker/pulls/fukamachi/qlot.svg)](https://hub.docker.com/r/fukamachi/qlot/)

```shell
$ docker run --rm -it fukamachi/qlot
```

You can build it by yourself with `docker build`:

```shell
$ git clone https://github.com/fukamachi/qlot
$ cd qlot
$ docker build -t qlot .
```

## Optional settings

### ASDF configuration to prevent from loading by mistake

ASDF loads any ASD files under a directory `~/common-lisp` including its subdirectories. It is easily understandable and convenient. However, it will lead to a problematic situation which ASDF accidentally loads libraries under ".qlot/" even in case you don't need it.

To avoid the situation, we recommend not to use `~/common-lisp` directory, or add the following lines to your Lisp's init file such as `~/.sbclrc` for SBCL.

```
;; .sbclrc
(push ".qlot" asdf::*default-source-registry-exclusions*)
(asdf:initialize-source-registry)
```

Roswell doesn't require this setting since it ignores directories starting with a dot.

## Tutorial

### Start using Qlot

```
$ qlot init
```

It creates 2 files "qlfile" and "qlfile.lock", and a directory ".qlot/" at the root of your project directory.

`qlfile` is a file clarifying the project dependencies. See [qlfile syntax](#qlfile-syntax) for the details.

`qlfile.lock` is similar to `qlfile` except the library versions are clarified. This will ensure that other developers or your deployment environment use exactly the same versions of libraries you just installed.

Make sure you add `qlfile` and `qlfile.lock` to your version-controlled repository.

```
$ git add qlfile qlfile.lock
$ git commit -m 'Start using Qlot.'
```

### Adding a new dependency

```
$ qlot add <library name>              # Add a new library from Quicklisp explicitly
$ qlot add <library name> --upstream   # Add an upstream version of a Quicklisp library
$ qlot add <username/repository>       # Add a new library from a GitHub repository
```

You can also edit a `qlfile` file directly. See [qlfile syntax](#qlfile-syntax) section to know how to write it. Be sure to run `qlot install` to install new dependencies.

### Updating the project-local Quicklisp

You can update the version of dependencies via:

```
$ qlot update

# Update a specific project
$ qlot update mito
$ qlot update mito sxql
```

It will also overwrite `qlfile.lock`.

## Commands

### install

`qlot install` will install Quicklisp and libraries that are declared in `qlfile` project-locally. `qlfile.lock` will be used with precedence if it exists.

```
$ qlot install
```

### update

`qlot update` will update the project-local `.qlot/` directory using `qlfile`.

```
$ qlot update

# Update a specific project
$ qlot update mito
$ qlot update mito sxql
```

### add

`qlot add` will add a line to `qlfile` and invoke `qlot install` internally. It replaces an existing line if a library with the same name already exists.

Its arguments are same as the qlfile syntax.

```
$ qlot add mito                                # ql mito
$ qlot add mito --latest                       # ql mito :latest
$ qlot add mito --upstream                     # ql mito :upstream
$ qlot add fukamachi/datafly                   # github datafly fukamachi/datafly
$ qlot add fukamachi/datafly --branch stable   # github datafly fukamachi/datafly :branch stable
$ qlot add ultralisp egao1980-cl-idna          # ultralisp egao1980-cl-idna
```

### remove

`qlot remove` will remove libraries from `qlfile` and invoke `qlot install` internally.

```
$ qlot remove mito
$ qlot remove mito datafly   # can specify multiple names
```

### exec

`qlot exec` does following:

* configures ASDF's source registries;
* adds Roswell's `bin` directory to the `PATH` environment variable;
* executes given command with arguments.

Here are few useful commands:

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

Qlot doesn't authenticate itself, but retrieving from private repository can be done via git's SSH key authentication. This means, if the current user can `git clone`, Qlot also would be possible to do it.

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

GitHub API may return 401 Forbidden when exceeding the rate limit ([API docs](https://docs.github.com/en/rest/using-the-rest-api/rate-limits-for-the-rest-api?apiVersion=2022-11-28)). It happens often because unauthenticated users can only make 60 requests per hour.

Setting an environment variable `GITHUB_TOKEN` to use a custom token raises it to 5,000 requests per hour.

### local

```
local <project name> <directory path>
```

Add a directory to the ASDF's source registry.

```
local rove ~/Programs/lib/rove
```

### dist

```
dist <distribution URL> [<dist version>]
```

`dist` allows to use a custom Quicklisp dist, like Ultralisp.

```
dist http://beta.quicklisp.org/dist/quicklisp.txt
dist http://dist.ultralisp.org/
```

## Priorities of distributions

If multiple distributions provide the same library, lower ones would take priority over higher ones.

## Working with SLIME

[SLIME](https://github.com/slime/slime) is the most popular development environment in Common Lisp. However, its REPL always loads the global Quicklisp, not the project-local one.

Here's quick steps to start project-local REPL with SLIME for each text editor:

### Lem

1. Add [lem-project/micros](https://github.com/lem-project/micros) to `.qlot/local-projects`.

```shell
$ git clone https://github.com/lem-project/micros .qlot/local-projects/micros
```

2. Add the following function to `~/.lem/init.lisp`.

```common-lisp
(define-command slime-qlot-exec () ()
  (let ((command (first (lem-lisp-mode/implementation::list-roswell-with-qlot-commands))))
    (when command
      (lem-lisp-mode:run-slime command))))
```

3. Relaunch the Lem, or reload the init file with `M-x lisp-load-file RET ~/.lem/init.lisp`.

4. Invoke `M-x slime-qlot-exec RET /path/to/project/`.

### Emacs

1. Add one of the following functions to `init.el`.

#### a) SLIME

```emacs-lisp
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))
```

See the [SLIME manual](https://slime.common-lisp.dev/doc/html/Multiple-Lisps.html#Multiple-Lisps) to set up multiple Lisps.

#### b) Sly

```emacs-lisp
(setq sly-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))
```

See the [Sly manual](https://joaotavora.github.io/sly/#Multiple-Lisps) to set up multiple Lisps.

2. Relaunch the Emacs or load the config file.
3. Invoke `M-- M-x slime RET qlot RET`.

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

`PROJECT_ROOT/.qlot/local-projects` can be used for local git repositories. Symbolic links are also accessible in Qlot environment.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## License

See [LICENSE.txt](LICENSE.txt).

The license of bundled Quicklisp installer can be found at [quicklisp/LICENSE.txt](quicklisp/LICENSE.txt).
