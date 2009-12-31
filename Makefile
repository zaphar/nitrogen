all: compile

compile:
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make

debug:
	mkdir -p ebin
	mkdir -p test/ebin
	ERL_LIBS=deps erl -noinput -run make all debug_info -run init stop

clean:
	rm -rf ./coverage/*.*
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*

test: compile
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-s test_suite test \
		-s init stop

coverage: compile
	git submodule init lib/coverize
	git submodule update lib/coverize
	make -C lib/coverize
	mkdir -p coverage
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-pa lib/coverize/ebin \
		-s eunit_helper run_cover \
		-s init stop
