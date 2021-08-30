# jupyterlab-ihaskell

Haskell Syntax Highlighting in Jupyterlab


## Prerequisites

* JupyterLab

## Installation

```bash
jupyter labextension install jupyterlab-ihaskell
```

## Development

For a development install (requires npm version 4 or later), do the following in the repository directory:

```bash
npm install
npm run build
jupyter labextension link .
```

To rebuild the package and the JupyterLab app:

```bash
npm run build
jupyter lab build
```

## Prebuilt extension

This is currently built with

```bash
$ nix-shell -p 'python3.withPackages(p: [ p.jupyterlab ])' nodejs
[nix-shell] $ npm install
[nix-shell] $ npm run build
[nix-shell] $ jupyter labextension build
```
