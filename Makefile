REBAR = ./rebar3
MIX = mix

compile-erl:
	@$(REBAR) compile

compile-ex: elixir
	@$(MIX) deps.get
	@$(MIX) compile

elixir:
	@$(REBAR) elixir generate_mix
	@$(REBAR) elixir generate_lib

tests:
	@$(REBAR) eunit

dist: release-ex release-erl

release: release-ex release-erl
	@$(REBAR) hex publish

release-erl: distclean-erl compile-erl tests

distclean-erl: distclean
	@rm -f rebar.lock

release-ex: distclean-ex compile-ex

distclean-ex: distclean
	@rm -f mix.lock

distclean:
	@rm -rf _build test/eunit deps

