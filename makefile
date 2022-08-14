
defualt:
	rustc -v a.rs -o pvic

help:
	clear
	rustc -v a.rs -o pvic
	./pvic --help hello.pvi

lex:
	clear
	rustc -v a.rs -o pvic
	./pvic --lex

format:
	clear
	rustc -v a.rs -o pvic
	./pvic --format

parse:
	clear
	rustc -v a.rs -o pvic
	./pvic --parse

check:
	clear
	rustc -v a.rs -o pvic
	./pvic --check

interpret:
	clear
	rustc -v a.rs -o pvic
	./pvic --interpret

