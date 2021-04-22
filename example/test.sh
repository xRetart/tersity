#! /bin/bash

# bring compiler up to date with debug mode enabled
cd ../compiler
make DEBUG=1
doas make install

# go to example project
cd ../example

# build the project
make

# run built executable
./target/executable

# print returned error code
echo -e "\nreturned $?"