ERL := erl
ERLC := erlc

COMMA := ,
EMPTY :=
SPACE := $(EMPTY) $(EMPTY)

TEST_SRC_FILES := $(wildcard test/*_tests.erl)
TESTS = $(patsubst test/%_tests.erl,%,$(TEST_SRC_FILES))
QTESTS := $(strip $(patsubst %,"%.beam",$(TESTS)))
TEST_FILES := $(subst $(SPACE),$(COMMA),$(QTESTS))

.PHONY : all compile test clean

compile :
	$(ERLC) -o ebin +debug_info erl_make.erl       
	$(ERL) -pa ./ebin -eval "erl_make:make(test)" -s init stop -noshell

all : clean compile test

test : compile

	$(ERL) -pa ./ebin \
	-eval 'eunit:test([$(TEST_FILES)], [verbose])' -s init stop -noshell
	
#	 -eval 'eunit:test([$(TEST_FILES)], [])' -s init stop -noshell
#To get verbose eunit output, just insert [verbose] like in this example:
#	 -eval 'eunit:test([$(TEST_FILES)], [verbose])' -s init stop -noshell
	 
	 
clean :
	rm -rf ebin/*.beam
