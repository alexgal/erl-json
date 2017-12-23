all: compile xref eunit dialyze edoc

rebar ?= rebar3
rebar_cmd = $(rebar) $(profile:%=as %)

compile:
	@$(rebar_cmd) compile

xref:
	@$(rebar_cmd) xref

clean:
	@$(rebar_cmd) clean

eunit:
	@$(rebar_cmd) do eunit

edoc: profile=edown
edoc:
	@$(rebar_cmd) edoc

run: compile
	-@$(rebar_cmd) shell