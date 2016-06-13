REBAR = ./rebar3

compile:
	$(verbose) $(REBAR) compile

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

tests:
	$(verbose) $(REBAR) eunit

dist: compile elixir

distclean:
	$(verbose) rm -rf _build rebar.lock mix.lock test/eunit deps

