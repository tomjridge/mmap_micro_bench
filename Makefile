run.exe: FORCE
	rm -f ./run.exe
	dune build run.exe
	cp _build/default/run.exe .

run: run.exe
	rm -f test.tmp
	./run.exe create
	sudo sh -c 'echo 3 >/proc/sys/vm/drop_caches'
#	./run.exe load
	./run.exe load_ic

clean:
	dune clean
	rm -f ./run.exe *.tmp
	rm -f ./dune-project

FORCE:
