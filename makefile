
defualt:
	rustc -v main.rs -o pvic

help:
	clear
	rustc -v main.rs -o pvic
	./pvic --help hello.pvi

lex:
	clear
	rustc -v main.rs -o pvic
	./pvic --lex

format:
	clear
	rustc -v main.rs -o pvic
	./pvic --format

parse:
	clear
	rustc -v main.rs -o pvic
	./pvic --parse

check:
	clear
	rustc -v main.rs -o pvic
	./pvic --check

interpret:
	clear
	rustc -v main.rs -o pvic
	./pvic --interpret

