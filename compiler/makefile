BASE_COMPILATION_FLAGS = \
	-std=c++2a -I/usr/include/llvm-11 -I/usr/include/llvm-c-11 \
	`llvm-config-11 --system-libs --libs core` -fno-rtti -fno-exceptions

LINKER_FLAGS = $(COMPILER_FLAGS)
CXX = clang++-11

# filename configuration
TARGET_DIRECTORY = target
TARGET = $(TARGET_DIRECTORY)/vector
SOURCE_FILE = source/main.cpp
OBJECT_FILE = $(TARGET_DIRECTORY)/main.o
LLVM_LIBRARY = /usr/bin/llvm-lib-11

HEADERS = $(shell find source/ -name "*.hpp")

# debug and release (default) configurations
ifeq ($(DEBUG), 1)
	COMPILER_FLAGS = $(BASE_COMPILATION_FLAGS) -Wall -Wextra -Wpedantic -Wimplicit-fallthrough -g -Og -v
else
	COMPILER_FLAGS = $(BASE_COMPILATION_FLAGS) -w -O3
endif


# implementation
ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

$(TARGET): $(OBJECT_FILE) $(LLVM_LIBRARY)
	$(CXX) $(OBJECT_FILE) -o $@ $(LINKER_FLAGS) && \
	strip -s $@

$(OBJECT_FILE): $(SOURCE_FILE) $(HEADERS)
	mkdir -p $(TARGET_DIRECTORY) && \
	$(CXX) $(SOURCE_FILE) -o $@ -c $(COMPILER_FLAGS)

install:
	install target/vector $(DESTDIR)$(PREFIX)/bin/

clean:
	rm -rf $(TARGET_DIRECTORY)