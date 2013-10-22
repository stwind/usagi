all: app

app: $(REBAR) deps
	@$(REBAR) compile

deps: $(REBAR) 
	@$(REBAR) get-deps

clean: $(REBAR)
	@$(REBAR) clean

test: $(REBAR) app
	@$(REBAR) eunit skip_deps=true $(if $(SUITES),suites=$(SUITES),)

ct: $(REBAR) app
	@$(REBAR) ct skip_deps=true

## copied from https://github.com/opscode/concrete
BASIC_PLT := ~/.dialyzer_plt
DIALYZER := dialyzer
DIALYZER_APPS = asn1 \
				compiler \
				crypto \
				edoc \
				erts \
				eunit \
				gs \
				hipe \
				inets \
				kernel \
				mnesia \
				observer \
				public_key \
				runtime_tools \
				ssl \
				stdlib \
				syntax_tools \
				tools

ALL_DEPS = $(notdir $(wildcard deps/*))
DEPS_LIST = $(filter-out $(DIALYZER_SKIP_DEPS), $(ALL_DEPS))
DIALYZER_DEPS = $(foreach dep,$(DEPS_LIST),deps/$(dep)/ebin)
DEPS_PLT = deps.plt
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: $(BASIC_PLT)
	@dialyzer $(DIALYZER_OPTS) -r ebin
else
dialyzer: $(BASIC_PLT) $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

$(BASIC_PLT):
	@echo "building $(BASIC_PLT) ..."
	@$(DIALYZER) --build_plt --output_plt $(BASIC_PLT) --apps $(DIALYZER_APPS)

xref: $(REBAR) app
	@$(REBAR) xref skip_deps=true

doc: $(REBAR) app
	@$(REBAR) doc skip_deps=true

lock-deps: $(REBAR) deps
	@$(REBAR) lock-deps skip_deps=true ignore=meck,moka,proper,rebar skip_dirs=rel

.PHONY: app doc deps clean test ct xref lock-deps dialyzer
