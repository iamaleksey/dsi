all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

shell:
	@erl -pa ../dsi/ebin

analyze:
	@./rebar analyze

checkplt:
	@./rebar check_plt

buildplt:
	@./rebar build_plt

xref:
	@./rebar xref

.PHONY: all compile clean shell analyze checkplt buildplt xref
