
defualt:
	rustc -v a.rs -o lall

help:
	clear
	rustc -v a.rs -o lall
	./lall --help hello.lal

lex:
	clear
	rustc -v a.rs -o lall
	./lall --lex

format:
	clear
	rustc -v a.rs -o lall
	./lall --format

parse:
	clear
	rustc -v a.rs -o lall
	./lall --parse

check:
	clear
	rustc -v a.rs -o lall
	./lall --check

interpret:
	clear
	rustc -v a.rs -o lall
	./lall --interpret

