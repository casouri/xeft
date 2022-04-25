.POSIX:
# Even if this is unnecessary, it doesn’t hurt.
PREFIX=/usr/local
CXX=g++
CXXFLAGS=-fPIC -I$(PREFIX)/include
LDFLAGS=-L$(PREFIX)/lib
LDLIBS=-lxapian

# Dylib extensions.
ifeq ($(OS),Windows_NT)
	SOEXT = dll
endif
ifeq ($(shell uname),Darwin)
	SOEXT = dylib
else
	SOEXT = so
endif

xapian-lite.$(SOEXT): module/xapian-lite.cc
	$(CXX) $< -o $@ -shared $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

clean:
	rm -f *.so *.o
