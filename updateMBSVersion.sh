#! /bin/bash

git submodule update --remote --merge --recursive && echo `date '+%Y/%m/%d_%H:%M:%S'` > assets/computercraft/lua/rom/.mbsVersion.txt && echo "success"