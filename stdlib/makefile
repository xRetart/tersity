# file name configuration (customizable by user)

# source files

# normal standard library sources
SOURCE_DIRECTORY = source
SOURCES = $(SOURCE_DIRECTORY)/bootstrap.vec

# system source files (system specific fundation for stdlib)
SYSTEM_SOURCE_DIRECTORY = $(SOURCE_DIRECTORY)/system
SYSTEM_SOURCES = $(shell find $(SYSTEM_SOURCE_DIRECTORY) -name *.asm)


# output files

# directory for building output
TARGET_DIRECTORY = target

# output of vector compiler (llvm intermediate representation)
LLVM_DIRECTORY = $(TARGET_DIRECTORY)/llvms
LLVM_FILES = $(LLVM_DIRECTORY)/bootstrap.ll

# directory for intermediate object files
OBJECT_DIRECTORY = $(TARGET_DIRECTORY)/objects
OBJECTS = $(OBJECT_DIRECTORY)/bootstrap.o

# same but for system objects
SYSTEM_OBJECT_DIRECTORY = $(OBJECT_DIRECTORY)/system
SYSTEM_OBJECTS = \
	$(SYSTEM_OBJECT_DIRECTORY)/exit.o \
	$(SYSTEM_OBJECT_DIRECTORY)/print_character.o

# build usable stdlib
TARGET = $(TARGET_DIRECTORY)/vector.a

CXX=clang-11
VECTOR=vector


# implementation, changes can break (maybe don't touch)

# assert environment variables
ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

# linking all object (system and stdlib) together
$(TARGET): $(OBJECTS) $(SYSTEM_OBJECTS)
	mkdir -p $(TARGET_DIRECTORY) && \
	ar rcs $@ $^

# compilation of stdlib source (vector format) to objects
$(OBJECTS): $(SOURCES)
	mkdir -p $(LLVM_DIRECTORY) && \
	$(VECTOR) $(SOURCES) $(LLVM_FILES) && \
	mkdir -p $(OBJECT_DIRECTORY) && \
	$(CXX) $(LLVM_FILES) -c -o $@

# compilation of sytem stdlib fundation sources (nasm format) to objects
$(SYSTEM_OBJECTS): $(SYSTEM_SOURCES)
	mkdir -p $(SYSTEM_OBJECT_DIRECTORY) && \
	for source in $(SYSTEM_SOURCES); \
	do \
		nasm -felf64 source -o $@; \
	done

install:
	install $(TARGET) $(DESTDIR)$(PREFIX)/lib/;

clean:
	rm -rf $(TARGET_DIRECTORY)

.PHONY: clean_intermediate
clean_intermediate:
	rm -rf $(LLVM_DIRECTORY)
	rm -rf $(OBJECT_DIRECTORY)