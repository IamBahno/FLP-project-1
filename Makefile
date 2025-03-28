# Define the path to the merlin ncursesw library
merlin_libncursesw_path = /lib/x86_64-linux-gnu/libncursesw.so.6.4

# Default target to build the project
.DEFAULT_GOAL := build

# Default build target
build:
	ghc src/*.hs -o flp-fun -Wall

# Build for merlin
build-merlin:
	rm -rf ./libs      # remove libs directory in case the last build failed and has not deleted it
	mkdir -p ./libs
	ln -s $(merlin_libncursesw_path) ./libs/libncursesw.so.5       # create symlink for the ncursesw library
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):./libs make build         # set the LD_LIBRARY_PATH and build
	rm -rf ./libs

clean:
	rm -rf ./libs
	rm -f src/*.hi src/*.o