#!/bin/bash

# first argument is the tag you want to search for
# all the following arguments are the tags files to search
cat ${@:2} | rg "^$1\t" | cut -f2 | sort | uniq
