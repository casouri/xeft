.POSIX:
PREFIX ?= /usr/local
CXX ?= g++
CXXFLAGS = -fPIC -I$(PREFIX)/include -std=c++11
LDFLAGS = -L$(PREFIX)/lib
LDLIBS = -lxapian

# Dylib extensions.
ifeq ($(OS),Windows_NT)
	SOEXT = dll
else ifeq ($(shell uname),Darwin)
	SOEXT = dylib
else
	SOEXT = so
endif

xapian-lite.$(SOEXT): xapian-lite.cc
	$(CXX) $< -o $@ -shared $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

clean:
	rm -f *.so *.o *.dylib *.dll
