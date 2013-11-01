REBAR=./rebar

build:
	@$(REBAR) get-deps compile

clean:
	@$(REBAR) clean

run-dev: build
	@sh priv/start-dev.sh
