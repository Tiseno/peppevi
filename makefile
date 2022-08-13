
defualt:
	clear
	rustc -v a.rs
	./a --format

lex:
	clear
	rustc -v a.rs
	./a --lex

parse:
	clear
	rustc -v a.rs
	./a --parse

check:
	clear
	rustc -v a.rs
	./a --check

