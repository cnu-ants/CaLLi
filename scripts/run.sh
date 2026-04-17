#!/bin/bash

# $1 = .bc path

rm result.txt
/home/yujina/repo/CaLLi/_build/default/example/analyzer.exe "$1" Func_main >result.txt
#rm filtered_result.txt
#./filter_result.sh

#rm output.json
#python3 make_json.py
