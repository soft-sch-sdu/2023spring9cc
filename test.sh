#!/bin/bash
try() {
  expected="$1"
  input="$2"

  ./leon9cc "$input" > tmp.s
  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" == "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input expected, but got $actual"
    exit 1
  fi
}

try 0 0
try 42 42
try 21 '5+20-4'
try 36 '1+2+3+4+5+6+7+8'
try 153 '1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17'
try 42 '3*7*1*2'
try 20 '3*7-1'
try 14 '4*7/2'

echo OK