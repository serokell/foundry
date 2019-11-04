.PHONY: build dev/build dev/watch dev/reset-db dev/run fmt tags clean

build:
	cabal v2-build --ghc-options "-Werror -O3"

dev/build:
	cabal v2-build

dev/watch:
	watchman-make -p '*.cabal' 'src/**/*.hs' -t dev/build

dev/run:
	cabal v2-run -- foundry "./expr.morte"

fmt:
	ormolu -c --mode inplace `find src -name "*.hs"`

tags:
	fast-tags -R src

clean:
	cabal v2-clean
