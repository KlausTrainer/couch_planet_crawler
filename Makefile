all:
	rebar get-deps compile

clean:
	rm -rf ebin doc erl_crash.dump
