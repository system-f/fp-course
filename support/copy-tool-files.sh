#!/bin/sh

if [ ! -f src/Course.hs ]; then
    echo You are running this from the wrong directory.
    echo Run it from the root of the repository.
    exit 1;
fi

cp -vf support/cabal.project cabal.project
cp -vf support/Setup.hs Setup.hs
cp -vf support/fp-course.cabal fp-course.cabal
cp -vf support/shell.nix shell.nix
cp -vf support/stack.yaml stack.yaml
cp -vf support/stack.yaml.lock stack.yaml.lock
