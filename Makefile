all: compile

shell:
	erl -pz ebin deps/*/ebin

compile:
	rebar compile skip_deps=true

deps:
	rebar get-deps

clean:
	rebar clean

test:
	rebar skip_deps=true eunit

start:
	erl -pz ebin deps/*/ebin -name erl_amon -eval "erl_amon:start(\"192.168.1.108\", 2464, \"production\")"

analyze:
	rebar analyze skip_deps=true

xref: compile
	rebar xref skip_deps=true
