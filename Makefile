.POSIX:
# Even if this is unnecessary, it doesn’t hurt.
PREFIX=/usr/local
CXX=g++
CXXFLAGS=-fPIC -I$(PREFIX)/include -std=c++11 -stdlib=libc++
LDFLAGS=-L$(PREFIX)/lib
LDLIBS=-lxapian

# Dylib extensions.
ifeq ($(OS),Windows_NT)
	SOEXT = dll
else ifeq ($(shell uname),Darwin)
	SOEXT = dylib
else
	SOEXT = so
endif

xapian-lite.$(SOEXT): module/xapian-lite.cc
	$(CXX) $< -o $@ -shared $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

module/xapian-lite.cc:
	git clone https://github.com/casouri/xapian-lite module --depth=1

clean:
	rm -f *.so *.o
	rm -rf module
