HAS_ELIXIR=1

include bu.mk

release: dist lint-release tag ##Publish a new release
	$(verbose) $(REBAR) hex publish

lint-release:
	$(verbose) $(REBAR) as release lint

