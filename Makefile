.PHONY: all compile shell

all: compile shell

compile:
	erlc -o ebin src/*.erl

shell: compile
	erl -pa ebin/
