# filename configuration

# vector source files
SOURCE_DIRECTORY = source
SOURCES = $(SOURCE_DIRECTORY)/main.vec


# build output files

# directory
BUILD_DIRECTORY = target

# general llvm intermediate representation
LLVMS_DIRECTORY = $(BUILD_DIRECTORY)/llvms
LLVMS = $(LLVMS_DIRECTORY)/general.ll

# platform specific object files
OBJECTS_DIRECTORY = $(BUILD_DIRECTORY)/objects
OBJECTS = $(OBJECTS_DIRECTORY)/platform_specific.o

# target executable
TARGET_DIRECTORY = $(BUILD_DIRECTORY)
TARGET = $(TARGET_DIRECTORY)/executable

# suite configuration
COMPILER = vector
STDLIB = vector.a


# implementation changes could break

# link object(s) together
$(TARGET): $(OBJECTS)
	mkdir -p $(BUILD_DIRECTORY)
	clang-11 $^ -o $@ && \
	strip -s $@

# compiler general llvm source(s) to specific object(s)
$(OBJECTS): $(LLVMS)
	mkdir -p $(OBJECTS_DIRECTORY)
	clang-11 $^ -c -o $@

# translate vector source(s) to llvm representation
$(LLVMS): $(SOURCES)
	mkdir -p $(LLVMS_DIRECTORY)
	$(COMPILER) $(SOURCES) $@

# remove all build files
clean:
	rm -rf $(TARGET_DIRECTORY)