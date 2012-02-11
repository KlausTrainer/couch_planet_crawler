all: log_dir
	rebar get-deps compile

clean:
	rm -rf ebin doc log erl_crash.dump

log_dir:
	@mkdir -p log
