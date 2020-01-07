HOME = ..
COREIR = $(HOME)/../coreir
LAKELIB = $(HOME)/../BufferMapping
LPATH = -L$(HOME)/lib -L$(LAKELIB)/cfunc/bin
INCS = -I$(HOME)/include -I. -I$(COREIR)/include -I$(LAKELIB)/cfunc/include

SRC=$(wildcard [^_]*.cpp)
OBJ=$(patsubst %.cpp, build/%.o, $(SRC))

LIBS= -lcoreir-commonlib -lcoreir -lcoreir-cgralib -lcoreirsim -lcoreir-float -lcoreir-lakelib

all: build/mapper

build/mapper: build/mapper.o
	$(MAKE) -C libs $(TARGET)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LPATH) $(LIBS) -static-libgcc -static-libstdc++
	cp build/mapper ../bin/cgra-mapper

build/%.o: %.cpp 
	$(CXX) $(CXXFLAGS) $(INCS) -c $^ -o $@

.PHONY: clean
clean:
	-rm -rf build/*
	$(MAKE) -C libs clean
