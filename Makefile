all: compile

compile:
	@@if [ ! -d ebin ]; then mkdir ebin; fi
	@erl -make
clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

test: compile
	@erl -pa ebin -eval 'ngram:train("data"), eunit:test(ngram), halt().'
	@erl -pa ebin -pa /usr/lib/ejabberd/ebin -eval 'ngram:init(), ngram:train("data"), eunit:test(chain_language), halt().'