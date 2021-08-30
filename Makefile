.POSIX:
PREFIX = /usr/local
CXX = g++
CXXFLAGS = -fPIC -I$(PREFIX)/include
LDFLAGS = -L$(PREFIX)/lib
LDLIBS = -lxapian

xeft-module.so: xeft-module.cc
	$(CXX) -shared $(CXXFLAGS) $(LDFLAGS) $(LDLIBS) $< -o $@

clean:
	rm -f *.so *.o
