
compile: clean
	@mkdir ebin
	@erlc -o ebin src/*.erl
	@$(MAKE) script

script:
	@echo "erl -noshell -pa ebin -s cauder start" > cauder.sh
	@chmod +x cauder.sh

docs: clean-docs
	@erl -noshell -run edoc_run files '["src/cauder.erl", \
	                                    "src/fwd_sem.erl", \
	                                    "src/bwd_sem.erl", \
	                                    "src/utils.erl"]' '[{dir, "docs"}]'

clean: clean-docs
	@rm -Rf ebin

clean-docs:
	@rm -Rf docs

debug: clean
	@mkdir ebin
	@erlc -Ddebug -o ebin src/*.erl
	@$(MAKE) script
