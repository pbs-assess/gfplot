#!/usr/bin/env sh

mkdir -p ~/.R

echo "CXXFLAGS += -std=c++11" >> ~/.R/Makevars
echo "CXX14 = g++ -fPIC"    >> ~/.R/Makevars
