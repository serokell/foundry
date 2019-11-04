# Godsmen Foundry

[![Build Status](https://dev.azure.com/int-index/Personal/_apis/build/status/serokell.foundry?branchName=master)](https://dev.azure.com/int-index/Personal/_build/latest?definitionId=3&branchName=master)
[![Build Status](https://travis-ci.org/serokell/foundry.svg?branch=master)](https://travis-ci.org/serokell/foundry)

Godsmen Foundry is a Morte IDE based on Source.

Implementation status:

* [x] rendering expressions
* [x] basic navigation
* [x] editing expressions
* [ ] interactive evaluation
* [ ] auto-completion
* [ ] local storage
* [ ] browsing Sigil

[sigil.place/tutorial/morte/1.7.1/even](http://sigil.place/tutorial/morte/1.7.1/even)
![Rendering Morte](examples/expr.svg)

## Getting Started

```
$ nix-build
$ result/bin/morte-to-sdam "./examples/expr.morte" > expr.sd
$ result/bin/sdam-to-svg --morte expr.sd
$ result/bin/foundry expr.sd
```

## Tooling

```
$ nix-shell -p haskellPackages.fast-tags haskellPackages.ormolu
$ make tags
$ make fmt
```

## SVG Optimization

```
$ nix-shell -p nodePackages.svgo
$ svgo expr.svg
```
