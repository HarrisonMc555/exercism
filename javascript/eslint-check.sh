#!/bin/bash

cd "/home/harrison/exercism/javascript/"
for f in $(find . \
                -mindepth 2 \
                -name '*.js' \
                -not -name '*.spec.js' \
                -not -path '*node_modules*'); do
    eslint "$f" && echo "no lint in $f";
done
