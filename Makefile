build:
	ghc --make -O2 -Wall -fno-warn-unused-do-bind Main.hs -o Main

clean:
	rm Main
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
