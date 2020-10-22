
compile: clean
	@mkdir ebin
	@erlc -o ebin src/*.erl
	@$(MAKE) script

script:
	@echo "erl -noshell -pa ebin -s cauder_wx start" > cauder.sh
	@chmod +x cauder.sh

docs: clean-docs
	@erl -noshell -run edoc_run files "[\"src/cauder.erl\", \
                                        \"src/cauder_eval.erl\", \
                                        \"src/cauder_load.erl\", \
                                        \"src/cauder_pp.erl\", \
                                        \"src/cauder_replay.erl\", \
                                        \"src/cauder_rollback.erl\", \
                                        \"src/cauder_semantics_backwards.erl\", \
                                        \"src/cauder_semantics_forwards.erl\", \
                                        \"src/cauder_syntax.erl\", \
                                        \"src/cauder_types.erl\", \
                                        \"src/cauder_utils.erl\", \
                                        \"src/cauder_wx.erl\", \
                                        \"src/cauder_wx_actions.erl\", \
                                        \"src/cauder_wx_areas.erl\", \
                                        \"src/cauder_wx_code.erl\", \
                                        \"src/cauder_wx_dialog.erl\", \
                                        \"src/cauder_wx_menu.erl\", \
                                        \"src/cauder_wx_process.erl\", \
                                        \"src/cauder_wx_statusbar.erl\", \
                                        \"src/cauder_wx_system.erl\", \
                                        \"src/cauder_wx_utils.erl\"]" "[{dir,\"docs\"}]"

clean: clean-docs
	@rm -Rf ebin

clean-docs:
	@rm -Rf docs

debug: clean
	@mkdir ebin
	@erlc -Ddebug -o ebin src/*.erl
	@$(MAKE) script
