build: ## Build the backend
	@cabal build -j -O0

clean: ## Remove the cabal build artifacts
	@cabal clean

repl: ## Start a cabal REPL
	@cabal repl lib:scrive-prelude

ghci: repl ## Start a cabal REPL (alias for `make repl`)

watch: ## Load the main library and reload on file change
	@ghcid --target lib:scrive-prelude

lint: ## Run the code linter
	@find src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code formatters
	@cabal-fmt -i *.cabal

tags:
	@ghc-tags -c

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

SHELL := /usr/bin/env bash

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
