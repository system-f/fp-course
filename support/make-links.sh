#!/bin/sh

if [ ! -f src/Course.hs ]; then
    echo You are running this from the wrong directory.
    echo Run it from the root of the repository.
    exit 1;
fi

ln -svf support/Setup.hs Setup.hs
ln -svf support/fp-course.cabal fp-course.cabal
ln -svf support/shell.nix shell.nix
ln -svf support/stack.yaml stack.yaml
ln -svf support/stack.yaml.lock stack.yaml.lock
