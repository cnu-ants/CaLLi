#!/bin/bash

# $1 = .bc path

rm result.txt
/home/yujina/repo/CaLLi/_build/default/example/analyzer.exe "$1" Func_main >result.txt