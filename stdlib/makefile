# file name configuration (customizable by user)

# source files

# normal standard library sources
SOURCE_DIRECTORY = source
SOURCES = $(SOURCE_DIRECTORY)/bootstrap.vec

# system source files (system specific fundation for stdlib)
SYSTEM_SOURCE_DIRECTORY = system
SYSTEM_SOURCES = $(shell find $(SOURCE_DIRECTORY)/$(SYSTEM_SOURCE_DIRECTORY) -name *.asm)


# output files

# directory for building output
TARGET_DIRECTORY = target

# output of vector compiler (llvm intermediate representation)
LLVM_DIRECTORY = llvm
LLVM_FILES = $(TARGET_DIRECTORY)/$(LLVM_DIRECTORY)/bootstrap.ll

# directory for intermediate object files
OBJECT_DIRECTORY = objects
OBJECTS = $(TARGET_DIRECTORY)/$(OBJECT_DIRECTORY)/bootstrap.o

# same but for system objects
SYSTEM_OBJECT_DIRECTORY = system
SYSTEM_OBJECTS = \
	$(TARGET_DIRECTORY)/$(OBJECT_DIRECTORY)/$(SYSTEM_OBJECT_DIRECTORY)/exit.o \
	$(TARGET_DIRECTORY)/$(OBJECT_DIRECTORY)/$(SYSTEM_OBJECT_DIRECTORY)/print_character.o

# build usable stdlib
TARGET = $(TARGET_DIRECTORY)/vector.a


# implementation, changes can break (maybe don't touch)

# assert environment variables
ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

# linking all object (system and stdlib) together
$(TARGET): $(OBJECTS) $(SYSTEM_OBJECTS)
	mkdir -p $(TARGET_DIRECTORY) && \
	ar rcs $@ $^ && \
	strip -s $@

# compilation of stdlib source (vector format) to objects
$(OBJECTS): $(SOURCES)
	mkdir -p $(TARGET_DIRECTORY)/$(LLVM_DIRECTORY) && \
	vector $(SOURCES) $(LLVM_FILES) && \
	mkdir -p $(TARGET_DIRECTORY)/$(OBJECT_DIRECTORY) && \
	clang-11 $(LLVM_FILES) -c -o $@

# compilation of sytem stdlib fundation sources (nasm format) to objects
$(SYSTEM_OBJECTS): $(SYSTEM_SOURCES)
	mkdir -p $(TARGET_DIRECTORY)/$(OBJECT_DIRECTORY)/$(SYSTEM_OBJECT_DIRECTORY) && \
	for source in $(SYSTEM_SOURCES); \
	do \
		nasm -felf64 source -o $@; \
	done

install:
	if [ -f target/vector.a ]; \
	then \
		install target/vector.a $(DESTDIR)$(PREFIX)/lib/; \
	else \
		echo "[make install] error: make before installing"; \
	fi

clean:
	rm -rf $(TARGET_DIRECTORY)

.PHONY: clean_intermediate
clean_intermediate:
	rm -rf $(TARGET_DIRECTORY)/$(LLVM_DIRECTORY)
	rm -rf $(TARGET_DIRECTORY)/$(OBJECT_DIRECTORY)
	rm -rf $(TARGET_DIRECTORY)/$(LLVM_DIRECTORY)