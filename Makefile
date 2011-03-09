all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	@./rebar eunit

shell:
	@erl -pa ../dsi/ebin

dialyze:
	@./rebar dialyze

checkplt:
	@./rebar check_plt

buildplt:
	@./rebar build_plt

xref:
	@./rebar xref

.PHONY: all compile clean test shell dialyze checkplt buildplt xref
