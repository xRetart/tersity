# vector
The home project for the vector programming language and associated utilities.

## inlcudes
	- compiler (compiler/)
	- standard library (stdlib/)
	- example/test (example/)
	
## version
0.0 yet to release

## installation
how to install all utilities needed for a running version
1. clone repository
	```shell
	git clone https://github.com/xRetart/vector.git
	```
2. build stdlib/
	```shell
	cd vector/stdlib && \
	make && \
	make install
	```
3. build compiler/
	```shell
	cd ../compiler && \
	make && \
	make install
	```
	
(optional) lastly the example can be tested to verify the build
	```shell
	cd ../example && \
	make && \
	./target/executable
	```

it should print "Hello, World!" without the quotes
