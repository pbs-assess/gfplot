#!/usr/bin/env sh

mkdir -p ~/.R

echo "CXX14FLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined" >> ~/.R/Makevars
echo "CXX14 = g++ -fPIC"    >> ~/.R/Makevars
echo "CXX14FLAGS+=-flto -Wno-unused-local-typedefs" >> ~/.R/Makevars
