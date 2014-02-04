
APP=gandalf

#REBAR = $(shell pwd)/rebar
REBAR=rebar
DEVS=dev1 dev2 dev3 #dev4 dev5 dev6 dev7 dev8
DEPSFILE=deps-riak_kv-1.4.7.tar.gz

.PHONY: deps rel stagedevrel

all: deps compile

compile:
	$(REBAR) compile

$(DEPSFILE):
	@echo
	@echo *** If file $(DEPSFILE) is not able to download, you can try '$(REBAR) get-deps'. ***
	@echo
	wget http://lastz.org/repo/legolas/$(DEPSFILE)

deps/riak_core: $(DEPSFILE)
	tar zxvf $(DEPSFILE) 

patch:
	patch -p0 -d deps < patches/patch-riak_kv-1.4.7.diff 
	patch -p0 -d deps < patches/patch-riak_core-1.4.4.diff 
	patch -p0 -d deps < patches/patch-eper-3280b736.diff

check_deps: deps/riak_core

deps: check_deps

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

test:
	$(REBAR) skip_deps=true eunit

rel: all
	$(REBAR) generate

relclean:
	rm -rf rel/${APP}

devrel: ${DEVS}

###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/${APP}/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/${APP}/lib;)


stagedevrel: ${DEVS}
	$(foreach dev,$^,\
	  $(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf dev/$(dev)/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) dev/$(dev)/lib;))

devrel: ${DEVS}


devclean:
	rm -rf dev

${DEVS}: all
	mkdir -p dev
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)


##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.${APP}_combo_dialyzer_plt

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		apps/*/ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		apps/*/ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) apps/*/ebin


cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
