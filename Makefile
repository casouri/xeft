.POSIX:
# Even if this is unnecessary, it doesn’t hurt.
PREFIX=/usr/local
CXX=g++
CXXFLAGS=-fPIC -I$(PREFIX)/include
LDFLAGS=-L$(PREFIX)/lib
LDLIBS=-lxapian

xapian-lite.so: module/xapian-lite.cc
	$(CXX) $< -o $@ -shared $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

clean:
	rm -f *.so *.o
