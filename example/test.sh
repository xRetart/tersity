#! /bin/bash

# cleanup to fully rebuild the project
make clean

# build the project
make

# run built executable
./target/executable

# print returned error code
echo -e "\nreturned $?"