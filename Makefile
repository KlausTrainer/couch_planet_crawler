all: log_dir
	./rebar get-deps compile

clean:
	rm -rf ebin doc log

log_dir:
	@mkdir -p log
