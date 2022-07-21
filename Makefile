run.exe: FORCE
	rm -f ./run.exe
	dune build run.exe
	cp _build/default/run.exe .

run1: run.exe
	rm -f test.tmp
	./run.exe create
	sudo sh -c 'echo 3 >/proc/sys/vm/drop_caches'
#	./run.exe load
	./run.exe load_ic

run2: run.exe
	rm -f test.tmp
	./run.exe test_map

run3: run.exe
	./run.exe test_map_marshal

clean:
	dune clean
	rm -f ./run.exe *.tmp *.m
	rm -f ./dune-project

FORCE:
