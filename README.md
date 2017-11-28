[![Melpa Status](http://melpa.milkbox.net/packages/sailfish-scratchbox-badge.svg)](http://melpa.milkbox.net/#/sailfish-scratchbox)
[![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/sailfish-scratchbox-badge.svg)](http://melpa-stable.milkbox.net/#/sailfish-scratchbox)
[![Build Status](https://travis-ci.org/vityafx/sailfish-scratchbox.el.svg?branch=master)](https://travis-ci.org/vityafx/sailfish-scratchbox.el)

# sailfish-scratchbox.el
Emacs helpers for working with sailfish os scratchbox (SDK).

## Contents
 * [Installation](#installation)
 * [Configuration](#configuration)
 * [Functions](#functions)

## Installation
The package can be installed via MELPA. The package name is `sailfish-scratchbox`.

## Configuration

The package contains pre-defined variables for easy working out-of-the-box but these
settings may not suit each user. You may customize these variables in your `.dir-locals.el` file:

```el
((nil . (
            (sailfish-scratchbox-interpreter . "bash -ic")
            (sailfish-scratchbox-which-sdk . "sdk")
            (sailfish-scratchbox-mb2-build . "mb2 build")
            (sailfish-scratchbox-mb2-build-options . "")
            (sailfish-scratchbox-build-buffer-name . "*scratchbox build*")
            (sailfish-scratchbox-deploy-buffer-name . "*scratchbox deploy*")
            (sailfish-scratchbox-deploy-rpms-command . "scp RPMS/*.rpm nemo@192.168.2.15:/home/nemo")
            (sailfish-scratchbox-install-in-sdk . "sb2 -R rpm -i RPMS/*.rpm --force --verbose")
            )))
```

Here are their descriptions:

- `sailfish-scratchbox-interpreter` - the interpreter to be used for invoking further scratchbox commands.
- `sailfish-scratchbox-which-sdk` - the command to open the sailfish sdk - according to the sailfish sdk
installation guide it is an alias `sdk` which is installed into `.bashrc` file.
- `sailfish-scratchbox-mb2-build` - the `mb2` build script for building the project.
- `sailfish-scratchbox-mb2-build-options` - additional options to be passed to `mb2` build script.
- `sailfish-scratchbox-build-buffer-name` - the build buffer name.
- `sailfish-scratchbox-deploy-buffer-name` - the build buffer name.
- `sailfish-scratchbox-deploy-rpms-command` - the command for copying project artifacts (rpm files) to the phone.
Must not ask user anything because there is no possibility for user input in the compilation buffer.
- `sailfish-scratchbox-install-in-sdk` - the command for installing project artifacts (rpm files) into the sdk (target).

## Functions
- `sailfish-scratchbox-mb2-build` - build the project in which the current buffer is in.
- `sailfish-scratchbox-deploy-rpms` - copy the project artifacts onto the phone.
- `sailfish-scratchbox-install-rpms` - install the project artifacts into the sdk (target).

## Usage
Using the package is very simple: open any file of a project in a buffer and then call the needed function - it will automatically find the project root and perform operations there.
