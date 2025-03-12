merlin_libncursesw_path = /lib/x86_64-linux-gnu/libncursesw.so.6.4

build:
	ghc src/*.hs -o flp-fun -Wall

build-merlin:
	mkdir -p ./libs
	ln -s $(merlin_libncursesw_path) ./libs/libncursesw.so.5
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):./libs make build
	rm -rf ./libs

clean:
	rm -rf ./libs