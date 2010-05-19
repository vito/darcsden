build:
	ghc --make -Wall Main.hs -o Main

# "continuous integration" - auto-recompile and restart whenever a module
# changes. sp is from searchpath.org , you might need the patched version
# from http://joyful.com/repos/searchpath .
BUILDFLAGS=-Wall
ci:
	sp --no-exts --no-default-map -o Main ghc --make Main.hs $(BUILDFLAGS) --run 

SOURCEFILES=`find . -name '*hs' -print -o -name Old -prune`
tag:
	rm -f TAGS; hasktags -e $(SOURCEFILES)

clean:
	rm Main
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
