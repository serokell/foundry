.PHONY: build dev/build dev/watch dev/reset-db dev/run tags clean

build:
	cabal v2-build --ghc-options "-Werror -O3"

dev/build:
	cabal v2-build

dev/watch:
	watchman-make -p '*.cabal' 'lib/**/*.hs' 'src/**/*.hs' -t dev/build

dev/run:
	cabal v2-run

tags:
	fast-tags -R lib src

clean:
	cabal v2-clean
