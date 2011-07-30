BUILDFLAGS=-isrc -Wall -fno-warn-unused-do-bind # -threaded

build:
	ghc --make $(BUILDFLAGS) src/WebServer.hs -o darcsden

# auto-recompile and restart whenever a module changes. sp is from
# http://joyful.com/repos/searchpath .
auto:
	sp --no-exts --no-default-map -o darcsden ghc --make WebServer.hs $(BUILDFLAGS) --run

SOURCEFILES=`find . -name '*hs' -print -o -name Old -prune`
tag:
	rm -f TAGS; hasktags -e $(SOURCEFILES)

clean:
	rm -f darcsden
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
