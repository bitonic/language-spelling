bench:
	ghc -O2 bench.hs

run-bench: bench
	./bench

clean:
	find . -name *.o | xargs rm -f
	find . -name *.hi | xargs rm -f
	rm -rf bench