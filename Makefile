REBAR=rebar

all:
	@$(REBAR) get-deps compile

clean_compile:
	@$(REBAR) clean compile

clean:
	@$(REBAR) clean

remove:
	@$(REBAR) delete-deps

get:
	@$(REBAR) get-deps

publish:
	@$(REBAR) generate

c:
	@$(REBAR) compile skip_deps=true
