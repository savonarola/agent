.PHONY: all compile run test clean
.PHONY: build_plt dialyzer

REBAR=./rebar3

DIALYZER_APPS = asn1 compiler crypto erts inets kernel public_key sasl ssl stdlib syntax_tools tools

all: $(REBAR) compile

compile:
		$(REBAR) compile

run:
		erl -pa _build/default/lib/*/ebin -boot start_sasl

test:
		$(REBAR) ct skip_deps=true verbose=3

clean:
		$(REBAR) clean
		rm -rf ./test/*.beam
		rm -rf ./erl_crash.dump
		rm -rf TEST*.xml

build_plt: clean compile
ifneq ("$(wildcard erlang.plt)","")
		@echo "Erlang plt file already exists"
else
		dialyzer --build_plt --output_plt erlang.plt --apps $(DIALYZER_APPS)
endif
ifneq ("$(wildcard agent.plt)","")
		@echo "agent plt file already exists"
else
		dialyzer --build_plt --output_plt agent.plt _build/default/lib/*/ebin
endif

add_to_plt: build_plt
		dialyzer --add_to_plt --plt erlang.plt --output_plt erlang.plt.new --apps $(DIALYZER_APPS)
		dialyzer --add_to_plt --plt agent.plt --output_plt agent.plt.new _build/default/lib/*/ebin
		mv erlang.plt.new erlang.plt
		mv agent.plt.new agent.plt

dialyzer:
		dialyzer --src src --plts erlang.plt agent.plt -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

