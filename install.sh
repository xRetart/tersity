#! /bin/bash


function build_install
{
	(cd $1 && make && make install)
}

# build and install compiler
build_install compiler

# build and install stdandard library
build_install stdlib