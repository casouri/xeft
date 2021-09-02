.POSIX:
PREFIX = /usr/local
CXX = g++
CXXFLAGS = -fPIC -I$(PREFIX)/include
LDFLAGS = -L$(PREFIX)/lib
LDLIBS = -lxapian

xeft-module.so: xeft-module.cc
	$(CXX) $< -o $@ -shared $(CXXFLAGS) $(LDFLAGS) $(LDLIBS)

clean:
	rm -f *.so *.o
