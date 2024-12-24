# default build (requires meson):
#	make compile PREFIX=/dest/dir TYPE={release,debug}
#	make install PREFIX=/dest/dir TYPE={release,debug}
#	make uninstall PREFIX=/dest/dir TYPE={release,debug}
PREFIX=/usr/local
TYPE=release
.PHONY: compile install uninstall
compile install:: builds/$(TYPE)/meson-info
	cd builds/$(TYPE) && meson $@
compile::
	ln -sf builds/$(TYPE)/rxgrep .
	ln -sf builds/$(TYPE)/tryit .
uninstall: builds/$(TYPE)/meson-info
	cd builds/$(TYPE) && ninja $@
builds/$(TYPE)/meson-info:
	meson setup builds/$(TYPE) --prefix=$(PREFIX) --buildtype=$(TYPE)

# traditional build (requires only make): make tryit
CFLAGS=-O3 -Wall
CXXFLAGS=-std=c++20 -O3 -Wall
rxgrep: minrx.o rxgrep.o
	$(CXX) -o $@ $^
tryit: minrx.o tryit.o
	$(CXX) -o $@ $^

# removes both default and traditional build artifacts
.PHONY: clean
clean:
	rm -fr builds *.o rxgrep tryit
