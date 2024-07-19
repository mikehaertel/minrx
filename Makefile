CFLAGS=-O
CXXFLAGS=-O -std=c++20

tryit: minrx.o tryit.o
	$(CXX) -o $@ $^

.PHONY: clean
clean:
	rm -f *.o tryit
