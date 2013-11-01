REBAR=./rebar

build:
	@$(REBAR) get-deps compile

clean:
	@$(REBAR) clean

run-dev:
	@sh priv/start-dev.sh
